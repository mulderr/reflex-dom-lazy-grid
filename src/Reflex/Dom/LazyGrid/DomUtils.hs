{-# LANGUAGE GADTs
  , FlexibleContexts
  , OverloadedStrings
  , RecursiveDo
  , ScopedTypeVariables
  , CPP #-}

module Reflex.Dom.LazyGrid.DomUtils
  ( resizeDetectorWithAttrs'
  , textInputClearable
  , triggerDownload
  , tshow
  ) where

import           Control.Lens ((^.))
import           Control.Monad (liftM, void)
import           Control.Monad.Fix
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import           GHCJS.DOM.Element hiding (drop)
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import           GHCJS.DOM.EventM (on)
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import           GHCJS.DOM.Types (MonadJSM, liftJSM)
import qualified GHCJS.DOM.Types as DOM

#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.DOM.Blob
import qualified GHCJS.DOM.Document as D
import           GHCJS.DOM.URL
#endif

import           Reflex
import           Reflex.Dom.Core

{-# INLINABLE tshow #-}
tshow :: Show a => a -> Text
tshow = T.pack . show

-- an HTML5 way of locally triggering a file download with arbitrary content
-- only tested on recent versions of Chrome and Firefox
triggerDownload
  :: DOM.Document
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
  DOM.click $ DOM.HTMLElement $ unElement a
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

-- Does not hardcode position: relative on the outer div. We need position: absolute.
-- Otherwise same as `resizeDetectorWithAttrs`.
{-# INLINABLE resizeDetectorWithAttrs' #-}
resizeDetectorWithAttrs' :: (MonadJSM m, DomBuilder t m, PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadJSM (Performable m), MonadFix m)
  => Map Text Text -- ^ A map of attributes. Warning: It should not modify the "position" style attribute.
  -> m a -- ^ The embedded widget
  -> m (Event t (), a) -- ^ An 'Event' that fires on resize, and the result of the embedded widget
resizeDetectorWithAttrs' attrs w = do
  let childStyle = "position: absolute; left: 0; top: 0;"
      containerAttrs = "style" =: "position: absolute; left: 0; top: 0; right: 0; bottom: 0; overflow: scroll; z-index: -1; visibility: hidden;"
  (parent, (expand, expandChild, shrink, w')) <- elAttr' "div" attrs $ do
    w' <- w
    elAttr "div" containerAttrs $ do
      (expand, (expandChild, _)) <- elAttr' "div" containerAttrs $ elAttr' "div" ("style" =: childStyle) $ return ()
      (shrink, _) <- elAttr' "div" containerAttrs $ elAttr "div" ("style" =: (childStyle <> "width: 200%; height: 200%;")) $ return ()
      return (expand, expandChild, shrink, w')
  let p = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw parent
      reset = do
        let e = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw expand
            s = _element_raw shrink
        eow <- DOM.getOffsetWidth e
        eoh <- DOM.getOffsetHeight e
        let ecw = eow + 10
            ech = eoh + 10
        setAttribute (_element_raw expandChild) ("style" :: Text) (childStyle <> "width: " <> T.pack (show ecw) <> "px;" <> "height: " <> T.pack (show ech) <> "px;")
        esw <- DOM.getScrollWidth e
        setScrollLeft e esw
        esh <- DOM.getScrollHeight e
        setScrollTop e esh
        ssw <- DOM.getScrollWidth s
        setScrollLeft s ssw
        ssh <- DOM.getScrollHeight s
        setScrollTop s ssh
        lastWidth <- DOM.getOffsetWidth p
        lastHeight <- DOM.getOffsetHeight p
        return (Just lastWidth, Just lastHeight)
      resetIfChanged ds = do
        pow <- DOM.getOffsetWidth p
        poh <- DOM.getOffsetHeight p
        if ds == (Just pow, Just poh)
          then return Nothing
          else fmap Just reset
  pb <- delay 0 =<< getPostBuild
  expandScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw expand) (`on` Events.scroll) $ return ()
  shrinkScroll <- wrapDomEvent (DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw shrink) (`on` Events.scroll) $ return ()
  size0 <- performEvent $ fmap (const $ liftJSM reset) pb
  rec resize <- performEventAsync $ fmap (\d cb -> (liftIO . cb) =<< liftJSM (resetIfChanged d)) $ tag (current dimensions) $ leftmost [expandScroll, shrinkScroll]
      dimensions <- holdDyn (Nothing, Nothing) $ leftmost [ size0, fmapMaybe id resize ]
  return (fmapMaybe void resize, w')
