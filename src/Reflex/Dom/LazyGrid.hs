{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TemplateHaskell, TypeFamilies, CPP #-}
module Reflex.Dom.LazyGrid
  ( module Reflex.Dom.LazyGrid, module Reflex.Dom.LazyGrid.Css, def, (&), (.~)
  ) where

import           Control.Lens ((^.), makeLenses)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default
import           Data.List (sortBy)
import           Data.Maybe (isJust)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Time.Clock (NominalDiffTime)
import           Data.Foldable (forM_)
import           Text.CSV

import           GHCJS.DOM.Element (getOffsetHeight)

import           Reflex
import           Reflex.Dom

import           Reflex.Dom.LazyGrid.Css
import           Reflex.Dom.LazyGrid.Utils
import           Reflex.Dom.LazyGrid.DomUtils

type Columns k v = Map k (Column k v)
type Rows k v = Map (k, k) v
type Filters k = Map k String

-- | Grid column.
data Column k v = Column
  { _colName :: String -- ^ column name
  , _colHeader :: String -- ^ column header
  , _colValue :: (k, k) -> v -> String -- ^ column string value for display, can use row key and value
  , _colCompare :: Maybe (v -> v -> Ordering) -- ^ ordering function
  , _colFilter :: Maybe (String -> Rows k v -> Rows k v) -- ^ filtering function
  , _colVisible :: Bool -- ^ initial visibility
  , _colAttrs :: Map String String -- ^ attrs applied to <th> and available for use in row action
  }

instance Eq (Column k v) where
  x == y = _colName x == _colName y

instance Show (Column k v) where
  show = _colName

instance Default (Column k v) where
  def = Column { _colName = ""
               , _colHeader = ""
               , _colValue = (\_ _ -> "")
               , _colCompare = Nothing
               , _colFilter = Nothing
               , _colVisible = True
               , _colAttrs = mempty
               }

-- | Column ordering.
data SortOrder
  = SortNone
  | SortAsc
  | SortDesc
  deriving (Eq, Show, Enum)

instance Default SortOrder where
  def = SortNone

nextSort :: SortOrder -> SortOrder
nextSort SortDesc = SortNone
nextSort s = succ s

data GridOrdering k = GridOrdering k SortOrder
  deriving (Eq, Show)

instance Default k => Default (GridOrdering k) where
  def = GridOrdering def def

data GridConfig t m k v
   = GridConfig { _gridConfig_attributes :: Dynamic t (Map String String) -- ^ resizeDetector <div> attributes
                , _gridConfig_tableTag :: String -- ^ table tag eg. <table>
                , _gridConfig_tableAttributes :: Dynamic t (Map String String) -- ^ table tag attrs
                , _gridConfig_rowHeight :: Int -- ^ row height in px
                , _gridConfig_extraRows :: Int -- ^ extra rows rendered on top and bottom
                , _gridConfig_debounce :: NominalDiffTime
                , _gridConfig_columns :: Dynamic t (Columns k v)
                , _gridConfig_rows :: Dynamic t (Rows k v)
                , _gridConfig_selectionStrategy :: (((k, k), v) -> Rows k v -> Rows k v)
                , _gridConfig_menuWidget :: (GridMenuConfig t k v -> m (GridMenu t k))
                , _gridConfig_headWidget :: (GridHeadConfig t k v -> m (GridHead t k))
                , _gridConfig_bodyWidget :: (GridBodyConfig t m k v -> m (GridBody t k v))
                , _gridConfig_rowAction :: (Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t))
                }

instance (MonadWidget t m, Ord k) => Default (GridConfig t m k v) where
  def = GridConfig { _gridConfig_attributes = constDyn ("class" =: "grid-container")
                   , _gridConfig_tableTag = "table"
                   , _gridConfig_tableAttributes = constDyn ("class" =: "grid-table")
                   , _gridConfig_rowHeight = 30
                   , _gridConfig_extraRows = 10
                   , _gridConfig_debounce = 0.01
                   , _gridConfig_columns = constDyn mempty
                   , _gridConfig_rows = constDyn mempty
                   , _gridConfig_selectionStrategy = selectNone
                   , _gridConfig_menuWidget = gridMenuSimple
                   , _gridConfig_headWidget = gridHeadSimple
                   , _gridConfig_bodyWidget = gridBodySimple
                   , _gridConfig_rowAction = gridRowSimple
                   }

data Grid t k v
   = Grid { _grid_columns :: Dynamic t (Columns k v)
          , _grid_columnsVisible :: Dynamic t (Columns k v)
          , _grid_rows :: Dynamic t (Rows k v)
          , _grid_rowsFiltered :: Dynamic t (Rows k v)
          , _grid_rowsSelected :: Dynamic t (Rows k v)
          }

data GridMenuConfig t k v
   = GridMenuConfig { _gridMenuConfig_columns :: Dynamic t (Columns k v)
                    , _gridMenuConfig_rows :: Dynamic t (Rows k v)
                    , _gridMenuConfig_rowsFiltered :: Dynamic t (Rows k v)
                    , _gridMenuConfig_rowsSelected :: Dynamic t (Rows k v)
                    }

data GridMenu t k
   = GridMenu { _gridMenu_export :: Event t ()
              , _gridMenu_exportVisible :: Event t ()
              , _gridMenu_exportSelected :: Event t ()
              , _gridMenu_columnVisibility :: Dynamic t (Map k (Dynamic t Bool))
              }

data GridHeadConfig t k v
   = GridHeadConfig { _gridHeadConfig_columns :: Dynamic t (Columns k v)
                    , _gridHeadConfig_ordering :: Dynamic t (GridOrdering k)
                    }

data GridHead t k
   = GridHead { _gridHead_columnFilters :: Dynamic t (Map k (Dynamic t String))
              , _gridHead_columnSorts :: Dynamic t (Map k (Event t k))
              }

data GridBodyConfig t m k v
   = GridBodyConfig { _gridBodyConfig_columns :: Dynamic t (Columns k v) -- ^ visible columns
                    , _gridBodyConfig_rows :: Dynamic t (Rows k v)
                    , _gridBodyConfig_window :: Dynamic t (Rows k v)
                    , _gridBodyConfig_selectedRows :: Dynamic t (Rows k v)
                    , _gridBodyConfig_containerAttrs :: Dynamic t (Map String String)
                    , _gridBodyConfig_rowAction :: (Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t))
                    }

data GridBody t k v
   = GridBody { _gridBody_tbody :: El t
              , _gridBody_rowSelectEvents :: Dynamic t (Map (k, k) (Event t ((k, k), v)))
              }

data GridWindow t k v
   = GridWindow { _gridWindow_firstIndex :: Dynamic t Int
                , _gridWindow_windowSize :: Dynamic t Int
                , _gridWindow_window :: Dynamic t (Rows k v)
                , _gridWindow_attributes :: Dynamic t (Map String String)
                }

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

-- | Default menu widget implementation.
gridMenuSimple :: (MonadWidget t m, Ord k) => GridMenuConfig t k v -> m (GridMenu t k)
gridMenuSimple (GridMenuConfig cols rows filtered selected) = el "div" $ do
  (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") $ return ()
  menuOpen <- toggle False $ domEvent Click menuToggle
  menuAttrs <- mapDyn (\o -> "class" =: ("grid-menu" <> if o then " grid-menu-open" else "")) menuOpen
  gm <- elDynAttr "div" menuAttrs $ elClass "ul" "grid-menu-list" $ do
    (exportEl, _) <- el' "li" $ text "Export all data as csv"
    (exportVisibleEl, _) <- el' "li" $ text "Export visible data as csv"
    (exportSelectedEl, _) <- el' "li" $ text "Export selected data as csv"
    toggles <- listWithKey cols $ \k dc ->
      sample (current dc) >>= \c -> el "div" $ do
        rec (toggleEl, _) <- elDynAttr' "li" attrs $ text $ _colHeader c
            dt <- toggle (_colVisible c) (domEvent Click toggleEl)
            attrs <- mapDyn (\v -> ("class" =: ("grid-menu-col " <> if v then "grid-menu-col-visible" else "grid-menu-col-hidden"))) dt
        return dt
    return $ GridMenu
      (domEvent Click exportEl)
      (domEvent Click exportVisibleEl)
      (domEvent Click exportSelectedEl)
      toggles
  exportCsv cols $ tag (current rows) $ _gridMenu_export gm
  exportCsv cols $ tag (current filtered) $ _gridMenu_exportVisible gm
  exportCsv cols $ tag (current selected) $ _gridMenu_exportSelected gm
  return gm

-- | Default head widget implementation.
gridHeadSimple :: (MonadWidget t m, Ord k) => GridHeadConfig t k v -> m (GridHead t k)
gridHeadSimple (GridHeadConfig cols ordering) = el "thead" $ el "tr" $ do
  controls <- listWithKey cols $ \k dc -> sample (current dc) >>= \c -> elAttr "th" (_colAttrs c) $ do
    -- header and sort controls
    let headerClass = maybe "grid-col-title" (const "grid-col-title grid-col-title-sort") $ _colCompare c
    sortAttrs <- mapDyn (toSortIndicatorAttrs k) ordering
    (sortEl, _) <- elAttr' "div" ("class" =: headerClass) $ do
      text $ _colHeader c
      elDynAttr "span" sortAttrs $ return ()
    let sortEvent = maybe never (const $ tag (constant k) $ domEvent Click sortEl) $ _colCompare c
    -- filter controls
    filter <- case _colFilter c of
      Just f -> do
        ti <- textInputClearable "grid-col-filter-clear-btn" (def & attributes .~ constDyn ("class" =: "grid-col-filter" ))
        return $ _textInput_value ti
      Nothing -> return $ constDyn ""
    return (filter, sortEvent)
  filters <- mapDyn (fmap fst) controls
  sortEvents <- mapDyn (fmap snd) controls
  return $ GridHead filters sortEvents
  where
    -- given column key k and GridOrdering k return sort indicator attrs for that column
    toSortIndicatorAttrs :: (Eq k) => k -> GridOrdering k -> Map String String
    toSortIndicatorAttrs k (GridOrdering ck v) = "class" =: ("grid-col-sort-icon" <> if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> " grid-col-sort-icon-asc"
             SortDesc -> " grid-col-sort-icon-desc"
      else "")

-- | Default body widget implementation.
gridBodySimple :: (MonadWidget t m, Ord k) => GridBodyConfig t m k v -> m (GridBody t k v)
gridBodySimple (GridBodyConfig cols rows window selected attrs rowAction) = do
  (tbody, sel) <- elAttr' "tbody" ("tabindex" =: "0") $ elDynAttr "x-rowgroup" attrs $ do
    -- widgetHold is (ab)used to trigger complete redraw if rows or columns change
    sel <- widgetHold (return $ constDyn mempty) $ fmap (const $ do
      -- we want to sample the columns exactly once for all rows we render
      cs <- sample $ current cols
      listWithKey window $ \k dv -> do
        v <- sample $ current dv
        r <- rowAction cs k v =<< mapDyn (isJust . Map.lookup k) selected
        return $ (k, v) <$ domEvent Click r
      ) $ leftmost [changed cols, changed rows]
    return $ joinDyn sel
  return $ GridBody tbody sel
  where changed = fmap (const ()) . updated

-- | Default row action.
gridRowSimple :: (MonadWidget t m) => Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t)
gridRowSimple cs k v dsel = do
  attrs <- forDyn dsel $ \s -> if s then ("class" =: "grid-row-selected") else mempty
  (el, _) <- elDynAttr' "tr" attrs $ forM_ cs $ \c -> elAttr "td" (_colAttrs c) $ text ((_colValue c) k v)
  return el

-- | Grid view.
grid :: forall t m k v . (MonadWidget t m, Ord k, Enum k, Default k) => GridConfig t m k v -> m (Grid t k v)
grid (GridConfig attrs tableTag tableAttrs rowHeight extra debounceDelay cols rows rowSelect gridMenu gridHead gridBody rowAction) = do
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
      gridState <- combineDyn4 (,,,) cols rows filters sortState
      xs <- gridManager $ updated gridState

      initHeightE <- performEvent $ fmap (toElHeight tbody) pb
      resizeE <- performEvent . fmap (toElHeight tbody) =<< debounceShield gridResizeEvent
      tbodyHeight <- holdDyn 0 $ fmap ceiling $ leftmost [resizeE, initHeightE]
      scrollTop <- holdDyn 0 =<< debounceShield (domEvent Scroll tbody)

      GridWindow _ _ window rowgroupAttrs <- gridWindowManager rowHeight extra tbodyHeight scrollTop xs

      cs <- mapDyn (Map.filter (== True)) (joinDynThroughMap $ _gridMenu_columnVisibility gmenu)
        >>= combineDyn (Map.intersectionWith (\c _ -> c)) cols
      selected <- mapDyn (leftmost . Map.elems) sel
        >>= foldDyn rowSelect mempty . switchPromptlyDyn

  return $ Grid cols cs rows xs selected

  where
    toElHeight el = const $ getOffsetHeight $ _el_element el

    -- if the delay is given to be 0 there is no point in calling debounce
    debounceShield :: forall b . Event t b -> m (Event t b)
    debounceShield = case debounceDelay of
                       0 -> return
                       _ -> debounce debounceDelay

    -- whenever we switch to another column SortOrder is reset to SortAsc
    toSortState :: Event t k -> m (Dynamic t (GridOrdering k))
    toSortState = foldDyn f def
      where f k (GridOrdering pk v) = GridOrdering k (if k == pk then (nextSort v) else SortAsc)

-- | No row selection.
selectNone :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectNone _ = id

-- | Single row selection.
selectSingle :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectSingle (k, v) sel = maybe (Map.singleton k v) (const mempty) $ Map.lookup k sel

-- | Multiple row selection.
selectMultiple :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectMultiple (k, v) sel = maybe (Map.insert k v sel) (const $ Map.delete k sel) $ Map.lookup k sel

toCsv :: Columns k v -> Rows k v -> String
toCsv cols rows = printCSV $ toFields <$> Map.toList rows
  where toFields (k, x) = fmap (\c -> _colValue c k x) cs
        cs = Map.elems cols

exportCsv :: MonadWidget t m => Dynamic t (Columns k v) -> Event t (Rows k v) -> m ()
exportCsv dcols e = do
  doc <- askDocument
#ifdef ghcjs_HOST_OS
  performEvent_ $ fmap (liftIO . triggerDownload doc "text/csv" "export.csv" . uncurry toCsv) $ attachDyn dcols e
#else
  performEvent_ $ fmap (const $ liftIO $ print "export only implemented for GHCJS") e
#endif

makeLenses ''Column
makeLenses ''GridConfig
makeLenses ''Grid
makeLenses ''GridMenuConfig
makeLenses ''GridMenu
makeLenses ''GridHeadConfig
makeLenses ''GridHead
makeLenses ''GridBodyConfig
makeLenses ''GridBody

instance HasAttributes (GridConfig t m k v) where
  type Attrs (GridConfig t m k v) = Dynamic t (Map String String)
  attributes = gridConfig_attributes
