{-# language
    OverloadedStrings
  , RecursiveDo
  , TemplateHaskell
  , TypeFamilies
#-}

module Reflex.Dom.LazyGrid.Types where

import           Control.Lens.TH (makeLenses)
import           Control.Monad (forM, forM_, join)
import           Data.Bool (bool)
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
import           Reflex.Dom.Core
import           Text.CSV (printCSV)

import           Reflex.Dom.LazyGrid.DomUtils

type Columns k v = Map k (Column k v)
type Rows k v = Map (k, k) v
type Filters k = Map k Text

-- | Grid column.
data Column k v = Column
  { _colHeader :: Text -- ^ column header
  , _colValue :: (k, k) -> v -> Text -- ^ column string value for display, can use row key and value
  , _colCompare :: Maybe (v -> v -> Ordering) -- ^ ordering function
  , _colFilter :: Maybe (Text -> Rows k v -> Rows k v) -- ^ filtering function
  , _colVisible :: Bool -- ^ initial visibility
  , _colAttrs :: Map Text Text -- ^ attrs applied to <th> and available for use in row action
  }

instance Default (Column k v) where
  def = Column { _colHeader = ""
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
   = GridConfig { _gridConfig_attributes :: Map Text Text -- ^ resizeDetector <div> attributes
                , _gridConfig_tableTag :: Text -- ^ table tag eg. <table>
                , _gridConfig_tableAttributes :: Dynamic t (Map Text Text) -- ^ table tag attrs
                , _gridConfig_rowHeight :: Int -- ^ row height in px
                , _gridConfig_extraRows :: Int -- ^ extra rows rendered on top and bottom
                , _gridConfig_columns :: Columns k v
                , _gridConfig_rows :: Dynamic t (Rows k v)
                , _gridConfig_selectionStrategy :: (Maybe ((k, k), v) -> Rows k v -> Rows k v)
                , _gridConfig_selectionClear :: Event t ()
                , _gridConfig_menuWidget :: (GridMenuConfig t k v -> m (GridMenu t k))
                , _gridConfig_headWidget :: (GridHeadConfig t k v -> m (GridHead t k))
                , _gridConfig_bodyWidget :: (GridBodyConfig t m k v -> m (GridBody t k v))
                , _gridConfig_rowAction :: (Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t))
                }

instance (MonadWidget t m, Ord k) => Default (GridConfig t m k v) where
  def = GridConfig { _gridConfig_attributes = "class" =: "grid-container"
                   , _gridConfig_tableTag = "table"
                   , _gridConfig_tableAttributes = constDyn ("class" =: "grid-table")
                   , _gridConfig_rowHeight = 30
                   , _gridConfig_extraRows = 10
                   , _gridConfig_columns = mempty
                   , _gridConfig_rows = constDyn mempty
                   , _gridConfig_selectionStrategy = selectNone
                   , _gridConfig_selectionClear = never
                   , _gridConfig_menuWidget = mkGridMenu
                   , _gridConfig_headWidget = mkGridHead
                   , _gridConfig_bodyWidget = mkGridBody
                   , _gridConfig_rowAction = mkGridRow
                   }

data Grid t k v
   = Grid { _grid_columns :: Columns k v
          , _grid_columnsVisible :: Dynamic t (Columns k v)
          , _grid_rows :: Dynamic t (Rows k v)
          , _grid_rowsFiltered :: Dynamic t (Rows k v)
          , _grid_rowsSelected :: Dynamic t (Rows k v)
          }

data GridMenuConfig t k v
   = GridMenuConfig { _gridMenuConfig_columns :: Columns k v
                    , _gridMenuConfig_rows :: Dynamic t (Rows k v)
                    , _gridMenuConfig_rowsFiltered :: Dynamic t (Rows k v)
                    , _gridMenuConfig_rowsSelected :: Dynamic t (Rows k v)
                    }

data GridMenu t k
   = GridMenu { _gridMenu_export :: Event t ()
              , _gridMenu_exportVisible :: Event t ()
              , _gridMenu_exportSelected :: Event t ()
              , _gridMenu_columnVisibility :: Map k (Dynamic t Bool)
              }

data GridHeadConfig t k v
   = GridHeadConfig { _gridHeadConfig_columns :: Dynamic t (Columns k v) -- ^ visible columns
                    , _gridHeadConfig_ordering :: Dynamic t (GridOrdering k)
                    }

data GridHead t k
   = GridHead { _gridHead_columnFilters :: Dynamic t (Map k (Dynamic t Text))
              , _gridHead_columnSorts :: Dynamic t (Map k (Event t k))
              }

data GridBodyConfig t m k v
   = GridBodyConfig { _gridBodyConfig_columns :: Dynamic t (Columns k v) -- ^ visible columns
                    , _gridBodyConfig_rows :: Dynamic t (Rows k v)
                    , _gridBodyConfig_window :: Dynamic t (Rows k v)
                    , _gridBodyConfig_selectedRows :: Dynamic t (Rows k v)
                    , _gridBodyConfig_containerAttrs :: Dynamic t (Map Text Text)
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
                , _gridWindow_attributes :: Dynamic t (Map Text Text)
                }

-- | No row selection.
{-# INLINABLE selectNone #-}
selectNone :: Ord k => Maybe ((k, k), v) -> Rows k v -> Rows k v
selectNone _ = id

-- | Single row selection.
{-# INLINABLE selectSingle #-}
selectSingle :: Ord k => Maybe ((k, k), v) -> Rows k v -> Rows k v
selectSingle (Just (k, v)) sel = maybe (Map.singleton k v) (const mempty) $ Map.lookup k sel
selectSingle Nothing _ = mempty

-- | Multiple row selection.
{-# INLINABLE selectMultiple #-}
selectMultiple :: Ord k => Maybe ((k, k), v) -> Rows k v -> Rows k v
selectMultiple (Just (k, v)) sel = maybe (Map.insert k v sel) (const $ Map.delete k sel) $ Map.lookup k sel
selectMultiple Nothing _ = mempty

------------------------------------------------------------------------------
-- Templates

-- | Default menu widget implementation.
{-# INLINABLE mkGridMenu #-}
mkGridMenu :: (MonadWidget t m, Ord k) => GridMenuConfig t k v -> m (GridMenu t k)
mkGridMenu (GridMenuConfig cols rows filtered selected) = el "div" $ do
  (menuToggle, _) <- elAttr' "div" ("class" =: "grid-menu-toggle") blank
  menuOpen <- toggle False $ domEvent Click menuToggle
  let menuAttrs = ffor menuOpen $ \o -> "class" =: ("grid-menu" <> bool "" " grid-menu-open" o)
  gm <- elDynAttr "div" menuAttrs $ elClass "ul" "grid-menu-list" $ do
    (exportEl, _) <- el' "li" $ text "Export all data as csv"
    (exportVisibleEl, _) <- el' "li" $ text "Export visible data as csv"
    (exportSelectedEl, _) <- el' "li" $ text "Export selected data as csv"
    toggles <- forM cols $ \c -> el "div" $ do
        rec (toggleEl, _) <- elDynAttr' "li" attrs $ text $ _colHeader c
            dt <- toggle (_colVisible c) (domEvent Click toggleEl)
            let attrs = ffor dt $ \v -> "class" =: ("grid-menu-col " <> bool "grid-menu-col-hidden" "grid-menu-col-visible" v)
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
  where toFields (k, x) = fmap (\c -> T.unpack $ _colValue c k x) cs
        cs = Map.elems cols

exportCsv :: MonadWidget t m => Columns k v -> Event t (Rows k v) -> m ()
exportCsv cols e = do
  doc <- askDocument
  performEvent_ $ (triggerDownload doc "text/csv" "export.csv" . toCsv cols) <$> e

-- | Default head widget implementation.
{-# INLINABLE mkGridHead #-}
mkGridHead :: (MonadWidget t m, Ord k) => GridHeadConfig t k v -> m (GridHead t k)
mkGridHead (GridHeadConfig visCols ordering) = el "thead" $ el "tr" $ do
  controls <- listWithKey visCols $ \k dc -> do
    let thAttrs = fmap _colAttrs dc
    elDynAttr "th" thAttrs $ do

      -- header and sort controls
      let attrs = ffor dc $ \c -> "class" =: maybe "grid-col-title" (const "grid-col-title grid-col-title-sort") (_colCompare c)
          sortAttrs = fmap (toSortIndicatorAttrs k) ordering
      (sortBtn, _) <- elDynAttr' "div" attrs $ do
        dynText $ fmap _colHeader dc
        elDynAttr "span" sortAttrs $ return ()
      let sortEvent = ffor dc $ \c -> maybe never (const $ tag (constant k) $ domEvent Click sortBtn) $ _colCompare c
          fattrs = ffor dc $ \c -> "class" =: "grid-col-filter" <> "style" =: maybe "display: none;" (const "display: block;") (_colFilter c)
      finput <- textInputClearable "grid-col-filter-clear-btn" $ def & attributes .~ fattrs

      return (_textInput_value finput, switchPromptlyDyn sortEvent)
  return $ GridHead (fmap (fmap fst) controls) (fmap (fmap snd) controls)
  where
    -- given column key k and GridOrdering k return sort indicator attrs for that column
    toSortIndicatorAttrs :: (Eq k) => k -> GridOrdering k -> Map Text Text
    toSortIndicatorAttrs k (GridOrdering ck v) = "class" =: ("grid-col-sort-icon" <> if ck == k
      then case v of
             SortNone -> ""
             SortAsc -> " grid-col-sort-icon-asc"
             SortDesc -> " grid-col-sort-icon-desc"
      else "")

-- | Default body widget implementation.
{-# INLINABLE mkGridBody #-}
mkGridBody :: (MonadWidget t m, Ord k) => GridBodyConfig t m k v -> m (GridBody t k v)
mkGridBody (GridBodyConfig cols rows window selected attrs rowAction) = do
  (tbody, sel) <- elAttr' "tbody" ("tabindex" =: "0") $ elDynAttr "x-rowgroup" attrs $ do
    -- widgetHold is (ab)used to trigger complete redraw if rows or columns change
    sel <- widgetHold (return $ constDyn mempty) $ (do
      cs <- sample $ current cols
      listWithKey window $ \k dv -> do
        v <- sample $ current dv
        r <- rowAction cs k v $ fmap (Map.member k) selected
        return $ (k, v) <$ domEvent Click r
      ) <$ leftmost [() <$ updated cols, () <$ updated rows]
    return $ join sel
  return $ GridBody tbody sel

-- | Default row action.
{-# INLINABLE mkGridRow #-}
mkGridRow :: (MonadWidget t m) => Columns k v -> (k, k) -> v -> Dynamic t Bool -> m (El t)
mkGridRow cs k v dsel = do
  let attrs = ffor dsel $ bool mempty ("class" =: "grid-row-selected")
  (e, _) <- elDynAttr' "tr" attrs $ forM_ cs $ \c -> elAttr "td" (_colAttrs c) $ text ((_colValue c) k v)
  return e

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
  type Attrs (GridConfig t m k v) = Map Text Text
  attributes = gridConfig_attributes
