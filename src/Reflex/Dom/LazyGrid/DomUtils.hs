{-# LANGUAGE OverloadedStrings, RecursiveDo, ScopedTypeVariables, CPP #-}

module Reflex.Dom.LazyGrid.DomUtils
  ( resizeDetectorDynAttr
  , textInputClearable
  , triggerDownload
  , tshow
  ) where

import           Control.Lens ((^.))
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           GHCJS.DOM.Element hiding (drop)
import           GHCJS.DOM.EventM (on)

#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.DOM.Blob
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import qualified GHCJS.DOM.HTMLElement as HE
import qualified GHCJS.DOM.Types as DOM
import           GHCJS.DOM.URL
#else
import qualified GHCJS.DOM.Types as DOM
#endif

import           Reflex
import           Reflex.Dom

{-# INLINABLE tshow #-}
tshow :: Show a => a -> Text
tshow = T.pack . show

-- an HTML5 way of locally triggering a file download with arbitrary content
-- only tested on recent versions of Chrome and Firefox
triggerDownload
  :: DOM.HTMLDocument
  -> String -- ^ mime type
  -> String -- ^ file name
  -> String -- ^ content
  -> IO ()
#ifdef ghcjs_HOST_OS
triggerDownload doc mime filename s = do
  windowUrl <- js_windowURL
  blob <- newBlob [s] (Nothing :: Maybe DOM.BlobPropertyBag)
  (url :: String) <- createObjectURL windowUrl blob
  a <- D.createElement doc ("a" :: String)
  setAttribute a ("style" :: String) ("display: none;" :: String)
  setAttribute a ("download" :: String) filename
  setAttribute a ("href" :: String) url
  HE.click $ HE.HTMLElement $ unElement a
  revokeObjectURL windowUrl url

-- for triggerDownload
-- cannot use newURL; createObjectURL is only defined for window.URL?
foreign import javascript unsafe "window[\"URL\"]"
        js_windowURL :: IO URL

#else
triggerDownload doc mime filename s = return ()
#endif

-- | Text input with a button to clear the value.
-- The button content ie. icon or text is to be defined through CSS using btnClass.
{-# INLINABLE textInputClearable #-}
textInputClearable :: MonadWidget t m => Text -> TextInputConfig t -> m (TextInput t)
textInputClearable btnClass tic =
  elAttr "div" ("style" =: "position: relative;") $ do
    rec (e, _) <- elDynAttr' "span" attrs $ return ()
        let clearE = domEvent Click e
        ti <- textInput $ tic & setValue .~ ("" <$ clearE)
        attrs <- holdDyn emptyAttrs $ leftmost [ emptyAttrs <$ clearE
                                               , fmap f $ ti ^. textInput_input
                                               ]
    return ti
  where
    emptyAttrs = "style" =: "visibility: hidden;"
    f s = case s of
            "" -> emptyAttrs
            _  -> ("class" =: btnClass)

-- more general version of resizeDetectorWithStyle
-- need to specify class
-- caller is responsible for somehow setting position: relative or position: absolute
{-# INLINABLE resizeDetectorDynAttr #-}
resizeDetectorDynAttr :: MonadWidget t m
  => Dynamic t (Map Text Text) -- ^ Element attributes. Warning: It must specifiy the "position" attribute with value either "absolute" or "relative".
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorDynAttr attrs w = do
  let childStyle = "position: absolute; left: 0; top: 0;" :: String
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elDynAttr' "div" attrs $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: T.pack childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (T.pack childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let reset = do
        let e = _element_raw expand
            s = _element_raw shrink
        eow <- getOffsetWidth e
        eoh <- getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        setAttribute (_element_raw expandChild) ("style" :: String) (childStyle <> "width: " <> show ecw <> "px;" <> "height: " <> show ech <> "px;")
        esw <- getScrollWidth e
        setScrollLeft e esw
        esh <- getScrollHeight e
        setScrollTop e esh
        ssw <- getScrollWidth s
        setScrollLeft s ssw
        ssh <- getScrollHeight s
        setScrollTop s ssh
        lastWidth <- getOffsetWidth (_element_raw parent)
        lastHeight <- getOffsetHeight (_element_raw parent)
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- getOffsetWidth (_element_raw parent)
        poh <- getOffsetHeight (_element_raw parent)
        if ds == (Just pow, Just poh)
           then return Nothing
           else liftM Just reset
  pb <- getPostBuild
  expandScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw expand) (`on` Events.scroll) $ return ()
  shrinkScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw shrink) (`on` Events.scroll) $ return ()
  size0 <- performEvent $ fmap (const $ liftIO reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> liftIO $ cb =<< resetIfChanged d) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmap (const ()) $ fmapMaybe id resize, w')
