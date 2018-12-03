{-# language
    OverloadedStrings
  , RecursiveDo
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
#-}

module Reflex.Dom.LazyGrid.Grid
  ( gridManager
  , gridFilter
  , gridSort
  , gridWindowManager
  , grid
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Align (alignWith)
import           Data.Default
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.These (these)
import           GHCJS.DOM.HTMLElement (getOffsetHeight)
import qualified GHCJS.DOM.Types as DOM
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.LazyGrid.DomUtils (resizeDetectorWithAttrs', tshow)
import           Reflex.Dom.LazyGrid.Types


-- | Handles model changes in response to filtering and sorting.
{-# INLINABLE gridManager #-}
gridManager :: (Reflex t, Ord k, Enum k)
  => Dynamic t (Columns k v, Rows k v, Filters k, GridOrdering k)
  -> Dynamic t (Rows k v)
gridManager = fmap f
  where
    f (cols, rows, fs, order) = gridSort cols order $ gridFilter cols fs rows

-- | Apply filters to a set of rows.
{-# INLINABLE gridFilter #-}
gridFilter :: Ord k => Columns k v -> Filters k -> Rows k v -> Rows k v
gridFilter cols0 fs xs0 = Map.foldrWithKey (applyOne cols0) xs0 fs
  where
    applyOne _ _ "" xs = xs
    applyOne cols k s xs = case Map.lookup k cols of
                             Nothing -> xs
                             Just c -> case _colFilter c of
                                         Just f -> f s xs
                                         Nothing -> xs

-- | Apply column sorting to a set of rows.
{-# INLINABLE gridSort #-}
gridSort :: (Ord k, Enum k) => Columns k v -> GridOrdering k -> Rows k v -> Rows k v
gridSort cols0 (GridOrdering k sortOrder) = Map.fromList . sort . Map.toList
  where
    sort = reindex . maybe id id (maybeSortFunc k cols0)
    maybeSortFunc k cols = Map.lookup k cols >>= _colCompare >>= \f ->
      let f' = (\(_, v1) (_, v2) -> f v1 v2)
      in case sortOrder of
        SortNone -> Nothing
        SortAsc -> return $ sortBy f'
        SortDesc -> return $ sortBy (flip f')
    reindex = zipWith (\n ((_, k2), v) -> ((n, k2), v)) [(toEnum 1)..]

-- | Keeps the window updated based on scroll position and body height.
{-# INLINABLE gridWindowManager #-}
gridWindowManager :: forall t m k v . (MonadWidget t m, Ord k)
                  => Int -- ^ row height in px
                  -> Int -- ^ extra row count
                  -> Dynamic t Int -- ^ body height
                  -> Dynamic t Int -- ^ scroll position
                  -> Dynamic t (Rows k v)
                  -> m (GridWindow t k v)
gridWindowManager rowHeight extra height scrollTop xs = do
  firstIndex <- holdUniqDyn =<< foldDyn toFirstIdx 0 (updated scrollTop)
  windowSize <- holdUniqDyn $ fmap toWindowSize height
  attrs <- holdUniqDyn $ zipDynWith toWindowAttrs firstIndex $ fmap Map.size xs
  let window = toWindow <$> firstIndex <*> windowSize <*> xs
  return $ GridWindow firstIndex windowSize window attrs

  where
    -- first index parity must be stable not to have the zebra "flip" when using css :nth-child
    toFirstIdx :: Int -> Int -> Int
    toFirstIdx scrollTop prev =
      let x = scrollTop `div` rowHeight - extra
          x' = if odd x then x - 1 else x
      in if abs (x' - prev) >= extra then x' else prev

    toWindowSize :: Int -> Int
    toWindowSize h =
      h `div` rowHeight + 1 + 2*extra

    toWindow :: Int -> Int -> Rows k v -> Rows k v
    toWindow firstIdx wsize =
      Map.fromList . take wsize . drop firstIdx . Map.toList

    -- the position of the window is given by two css properties:
    -- - top    - offset from the top
    -- - height - includes content height and offset from the bottom
    -- the main invariant being:
    --   rowCount * rowHeight = top + height
    toWindowAttrs :: Int -> Int -> Map Text Text
    toWindowAttrs firstIdx rowCount =
      let total = rowCount * rowHeight
          woffset = cutAtZero $ firstIdx * rowHeight
          wheight = total - woffset
          cutAtZero x = if x < 0 then 0 else x
      in toStyleAttr $ "position" =: "relative"
                    <> "overflow" =: "hidden"
                    <> "top"      =: (tshow woffset <> "px")
                    <> "height"   =: (tshow wheight <> "px")
      where
        toStyleAttr m = "style" =: (Map.foldrWithKey (\k v s -> k <> ":" <> v <> ";" <> s) "" m)

-- | Grid view.
{-# INLINABLE grid #-}
grid :: forall t m k v . (MonadWidget t m, Ord k, Enum k, Default k) => GridConfig t m k v -> m (Grid t k v)
grid (GridConfig attrs tableTag tableAttrs rowHeight extra cols rows rowSelect clearSelE gridMenu gridHead gridBody rowAction) = do
  rec (gridResizeEvent, (table, gmenu, ghead, (GridBody tbody sel))) <- resizeDetectorWithAttrs' attrs $ do
        gmenu <- gridMenu $ GridMenuConfig cols rows xs selected
        (table, (ghead, gbody)) <- elDynAttr' tableTag tableAttrs $ do
          ghead <- gridHead $ GridHeadConfig cs sortState
          gbody <- gridBody $ GridBodyConfig cs rows window selected rowgroupAttrs rowAction
          return (ghead, gbody)
        return (table, gmenu, ghead, gbody)

      sortState <- toSortState $ (switch . current) $ fmap (leftmost . Map.elems) (_gridHead_columnSorts ghead)
      let filters = joinDynThroughMap $ _gridHead_columnFilters ghead
          gridState = (,,,) cols <$> rows <*> filters <*> sortState
          xs = gridManager gridState

      pb <- getPostBuild
      rec initE <- delay 0.1 $ leftmost [pb, gate (fmap (== 0) $ current tbodyHeight) initE]
          initHeightE <- performEvent $ elHeight tbody <$ initE
          resizeE <- performEvent $ elHeight tbody <$ gridResizeEvent
          tbodyHeight <- holdUniqDyn =<< holdDyn 0 (fmap ceiling $ alignWith (these id id max) resizeE initHeightE)
      scrollTop <- holdDyn 0 $ fmap floor $ domEvent Scroll tbody

      -- debugging
      -- performEvent_ $ ffor initHeightE $ \x -> liftIO $ putStrLn $ "init: " ++ show x
      -- performEvent_ $ ffor resizeE $ \x -> liftIO $ putStrLn $ "resize: " ++ show x

      GridWindow _ _ window rowgroupAttrs <- gridWindowManager rowHeight extra tbodyHeight scrollTop xs

      let cs = fmap (Map.intersectionWith (\c _ -> c) cols) $ fmap (Map.filter (== True))
                 $ joinDynThroughMap $ constDyn $ _gridMenu_columnVisibility gmenu
          selE :: Event t ((k,k), v) = switch . current $ fmap (leftmost . Map.elems) sel

      -- A Nothing event clears the selection
      selected <- foldDyn rowSelect mempty $ leftmost [ Just <$> selE
                                                      , Nothing <$ clearSelE
                                                      , Nothing <$ updated rows
                                                      ]

  return $ Grid cols cs rows xs selected

  where
    elHeight = getOffsetHeight . DOM.uncheckedCastTo DOM.HTMLElement . _element_raw

    -- whenever we switch to another column SortOrder is reset to SortAsc
    toSortState :: Event t k -> m (Dynamic t (GridOrdering k))
    toSortState = foldDyn f def
      where f k (GridOrdering pk v) = GridOrdering k (if k == pk then nextSort v else SortAsc)
