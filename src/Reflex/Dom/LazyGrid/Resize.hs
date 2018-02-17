{-# LANGUAGE GADTs #-}

module Reflex.Dom.LazyGrid.Resize where

import Control.Lens ((^.))
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom.Core

-- | Create an Event that is fired whenever an element
resizeObserver :: (DomBuilder t m, TriggerEvent t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Element EventResult (DomBuilderSpace m) t -> m (Event t ())
resizeObserver observedEl = do
  (resizeE, triggerResizeE) <- newTriggerEvent
  liftJSM $ do
    cb <- asyncFunction $ \_ _ _ -> triggerResizeE ()
    ro <- new (jsg "ResizeObserver") [cb]
    let e = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw observedEl
    ro ^. js1 "observe" e
  return resizeE
