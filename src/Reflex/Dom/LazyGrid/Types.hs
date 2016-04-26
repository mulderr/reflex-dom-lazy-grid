{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.Dom.LazyGrid.Types where

import           Control.Lens ((^.), makeLenses)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool (bool)
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Maybe (isJust)
import           Reflex
import           Reflex.Dom
import           Text.CSV (printCSV)

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
                   , _gridConfig_columns = constDyn mempty
                   , _gridConfig_rows = constDyn mempty
                   , _gridConfig_selectionStrategy = selectNone
                   , _gridConfig_menuWidget = mkGridMenu
                   , _gridConfig_headWidget = mkGridHead
                   , _gridConfig_bodyWidget = mkGridBody
                   , _gridConfig_rowAction = mkGridRow
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

-- | No row selection.
selectNone :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectNone _ = id

-- | Single row selection.
selectSingle :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectSingle (k, v) sel = maybe (Map.singleton k v) (const mempty) $ Map.lookup k sel

-- | Multiple row selection.
selectMultiple :: Ord k => ((k, k), v) -> Rows k v -> Rows k v
selectMultiple (k, v) sel = maybe (Map.insert k v sel) (const $ Map.delete k sel) $ Map.lookup k sel

------------------------------------------------------------------------------
-- Templates

-- | Default menu widget implementation.
mkGridMenu :: (MonadWidget t m, Ord k) => GridMenuConfig t k v -> m (GridMenu t k)
mkGridMenu (GridMenuConfig cols rows filtered selected) = el "div" $ do
  (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") blank
  menuOpen <- toggle False $ domEvent Click menuToggle
  menuAttrs <- forDyn menuOpen $ \o -> "class" =: ("grid-menu" <> bool "" " grid-menu-open" o)
  gm <- elDynAttr "div" menuAttrs $ elClass "ul" "grid-menu-list" $ do
    (exportEl, _) <- el' "li" $ text "Export all data as csv"
    (exportVisibleEl, _) <- el' "li" $ text "Export visible data as csv"
    (exportSelectedEl, _) <- el' "li" $ text "Export selected data as csv"
    toggles <- listWithKey cols $ \k dc ->
      sample (current dc) >>= \c -> el "div" $ do
        rec (toggleEl, _) <- elDynAttr' "li" attrs $ text $ _colHeader c
            dt <- toggle (_colVisible c) (domEvent Click toggleEl)
            attrs <- forDyn dt $ \v -> "class" =: ("grid-menu-col " <> bool "grid-menu-col-hidden" "grid-menu-col-visible" v)
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

toCsv :: Columns k v -> Rows k v -> String
toCsv cols rows = printCSV $ toFields <$> Map.toList rows
  where toFields (k, x) = fmap (\c -> _colValue c k x) cs
        cs = Map.elems cols

exportCsv :: MonadWidget t m => Dynamic t (Columns k v) -> Event t (Rows k v) -> m ()
exportCsv dcols e = do
  doc <- askDocument
#ifdef ghcjs_HOST_OS
  performEvent_ $ (liftIO . triggerDownload doc "text/csv" "export.csv" . uncurry toCsv) <$> attachDyn dcols e
#else
  performEvent_ $ (liftIO $ print "export only implemented for GHCJS") <$ e
#endif

-- | Default head widget implementation.
mkGridHead :: (MonadWidget t m, Ord k) => GridHeadConfig t k v -> m (GridHead t k)
mkGridHead (GridHeadConfig cols ordering) = el "thead" $ el "tr" $ do
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
mkGridBody :: (MonadWidget t m, Ord k) => GridBodyConfig t m k v -> m (GridBody t k v)
mkGridBody (GridBodyConfig cols rows window selected attrs rowAction) = do
  (tbody, sel) <- elAttr' "tbody" ("tabindex" =: "0") $ elDynAttr "x-rowgroup" attrs $ do
    -- widgetHold is (ab)used to trigger complete redraw if rows or columns change
    sel <- widgetHold (return $ constDyn mempty) $ (do
      -- we want to sample the columns exactly once for all rows we render
      cs <- sample $ current cols
      listWithKey window $ \k dv -> do
        v <- sample $ current dv
        r <- rowAction cs k v =<< mapDyn (isJust . Map.lookup k) selected
        return $ (k, v) <$ domEvent Click r
      ) <$ leftmost [() <$ updated cols, () <$ updated rows]
    return $ joinDyn sel
  return $ GridBody tbody sel

-- | Default row action.
mkGridRow :: (MonadWidget t m) => Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t)
mkGridRow cs k v dsel = do
  attrs <- forDyn dsel $ bool mempty ("class" =: "grid-row-selected")
  (el, _) <- elDynAttr' "tr" attrs $ forM_ cs $ \c -> elAttr "td" (_colAttrs c) $ text ((_colValue c) k v)
  return el

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
