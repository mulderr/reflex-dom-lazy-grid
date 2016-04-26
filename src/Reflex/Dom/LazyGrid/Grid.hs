{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.LazyGrid.Grid 
  ( gridManager
  , gridFilter
  , gridSort
  , gridWindowManager
  , grid
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           GHCJS.DOM.Element (getOffsetHeight)
import           Reflex
import           Reflex.Dom

import           Reflex.Dom.LazyGrid.DomUtils (resizeDetectorDynAttr)
import           Reflex.Dom.LazyGrid.Types
import           Reflex.Dom.LazyGrid.Utils


-- | Handles model changes in response to filtering and sorting.
gridManager :: (MonadWidget t m, Ord k, Enum k)
  => Event t (Columns k v, Rows k v, Filters k, GridOrdering k)
  -> m (Dynamic t (Rows k v))
gridManager = holdDyn mempty . fmap f
  where
    f (cols, rows, fs, order) = gridSort cols order $ gridFilter cols fs rows

-- | Apply filters to a set of rows.
gridFilter :: Ord k => Columns k v -> Filters k -> Rows k v -> Rows k v
gridFilter cols fs xs = Map.foldrWithKey (applyOne cols) xs fs
  where
    applyOne _ _ "" xs = xs
    applyOne cols k s xs = case Map.lookup k cols of
                             Nothing -> xs
                             Just c -> case _colFilter c of
                                         Just f -> f s xs
                                         Nothing -> xs

-- | Apply column sorting to a set of rows.
gridSort :: (Ord k, Enum k) => Columns k v -> GridOrdering k -> Rows k v -> Rows k v
gridSort cols (GridOrdering k sortOrder) = Map.fromList . sort . Map.toList
  where
    sort = reindex . maybe id id (maybeSortFunc k cols)
    maybeSortFunc k cols = Map.lookup k cols >>= _colCompare >>= \f ->
      let f' = (\(_, v1) (_, v2) -> f v1 v2)
      in case sortOrder of
        SortNone -> Nothing
        SortAsc -> return $ sortBy f'
        SortDesc -> return $ sortBy (flip f')
    reindex = zipWith (\n ((_, k2), v) -> ((n, k2), v)) [(toEnum 1)..]

-- | Keeps the window updated based on scroll position and body height.
gridWindowManager :: forall t m k v . (MonadWidget t m, Ord k)
                  => Int -- ^ row height in px
                  -> Int -- ^ extra row count
                  -> Dynamic t Int -- ^ body height
                  -> Dynamic t Int -- ^ scroll position
                  -> Dynamic t (Rows k v)
                  -> m (GridWindow t k v)
gridWindowManager rowHeight extra height scrollTop xs = do
  firstIndex <- (return . nubDyn) =<< foldDyn toFirstIdx 0 (updated scrollTop)
  windowSize <- (return . nubDyn) =<< mapDyn toWindowSize height
  window <- combineDyn3 toWindow firstIndex windowSize xs
  attrs <- (return . nubDyn) =<< combineDyn toWindowAttrs firstIndex =<< mapDyn Map.size xs
  return $ GridWindow firstIndex windowSize window attrs
  where
    -- first index parity must be stable not to have the zebra "flip" when using css :nth-child
    toFirstIdx :: Int -> Int -> Int
    toFirstIdx scrollTop prev =
      let x = scrollTop `div` rowHeight - extra
          x' = if odd x then x - 1 else x
      in if abs (x' - prev) >= extra then x' else prev

    toWindowSize :: Int -> Int
    toWindowSize height =
      height `div` rowHeight + 1 + 2*extra

    toWindow :: Int -> Int -> Rows k v -> Rows k v
    toWindow firstIdx wsize =
      Map.fromList . take wsize . drop firstIdx . Map.toList

    -- the position of the window is given by two css properties:
    -- - top    - offset from the top
    -- - height - includes content height and offset from the bottom
    -- the main invariant being:
    --   rowCount * rowHeight = top + height
    toWindowAttrs :: Int -> Int -> Map String String
    toWindowAttrs firstIdx rowCount =
      let total = rowCount * rowHeight
          woffset = cutAtZero $ firstIdx * rowHeight
          wheight = total - woffset
          cutAtZero x = if x < 0 then 0 else x
      in toStyleAttr $ "position" =: "relative"
                    <> "overflow" =: "hidden"
                    <> "top"      =: (show woffset <> "px")
                    <> "height"   =: (show wheight <> "px")
      where
        toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

-- | Grid view.
grid :: forall t m k v . (MonadWidget t m, Ord k, Enum k, Default k) => GridConfig t m k v -> m (Grid t k v)
grid (GridConfig attrs tableTag tableAttrs rowHeight extra cols rows rowSelect gridMenu gridHead gridBody rowAction) = do
  pb <- getPostBuild
  rec (gridResizeEvent, (table, gmenu, ghead, (GridBody tbody sel))) <- resizeDetectorDynAttr attrs $ do
        gmenu <- gridMenu $ GridMenuConfig cols rows xs selected
        (table, (ghead, gbody)) <- elDynAttr' tableTag tableAttrs $ do
          ghead <- gridHead $ GridHeadConfig cs sortState
          gbody <- gridBody $ GridBodyConfig cs rows window selected rowgroupAttrs rowAction
          return (ghead, gbody)
        return (table, gmenu, ghead, gbody)

      -- TODO:
      -- if the old set of filteres is completely contained within the new we can keep existing work and
      -- only search within current xs
      --
      -- note we cannot avoid starting from scratch when we subtract something from any of the filters
      let filters = joinDynThroughMap $ _gridHead_columnFilters ghead
      sortState <- toSortState . switchPromptlyDyn =<< mapDyn (leftmost . Map.elems) (_gridHead_columnSorts ghead)
      gridState <- combineDyn3 ((,,,) cols) rows filters sortState
      xs <- gridManager $ updated gridState

      initHeightE <- performEvent $ elHeight tbody <$ pb
      resizeE <- performEvent $ elHeight tbody <$ gridResizeEvent
      tbodyHeight <- holdDyn 0 $ ceiling <$> leftmost [resizeE, initHeightE]
      scrollTop <- holdDyn 0 $ domEvent Scroll tbody

      GridWindow _ _ window rowgroupAttrs <- gridWindowManager rowHeight extra tbodyHeight scrollTop xs

      cs <- mapDyn (Map.filter (== True)) (joinDynThroughMap $ constDyn $ _gridMenu_columnVisibility gmenu)
        >>= mapDyn (Map.intersectionWith (\c _ -> c) cols)

      selected <- mapDyn (leftmost . Map.elems) sel
        >>= foldDyn rowSelect mempty . switchPromptlyDyn

  return $ Grid cols cs rows xs selected

  where
    elHeight = getOffsetHeight . _el_element

    -- whenever we switch to another column SortOrder is reset to SortAsc
    toSortState :: Event t k -> m (Dynamic t (GridOrdering k))
    toSortState = foldDyn f def
      where f k (GridOrdering pk v) = GridOrdering k (if k == pk then nextSort v else SortAsc)
