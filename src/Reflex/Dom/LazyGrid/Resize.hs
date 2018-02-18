{-# LANGUAGE GADTs #-}

module Reflex.Dom.LazyGrid.Resize where

import Control.Lens ((^.))
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom.Core

data DomRect
  = DomRect
  { _domRect_x :: Double
  , _domRect_y :: Double
  , _domRect_width :: Double
  , _domRect_height :: Double
  , _domRect_top :: Double
  , _domRect_right :: Double
  , _domRect_bottom :: Double
  , _domRect_left :: Double
  } deriving (Eq, Show)

-- | Create an Event that is fired whenever an element is resized (via ResizeObserver API)
resizeObserver :: (DomBuilder t m, TriggerEvent t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
  => Element EventResult (DomBuilderSpace m) t -> m (Event t DomRect)
resizeObserver observedEl = do
  let e = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw observedEl
  (resizeE, triggerResizeE) <- newTriggerEvent
  liftJSM $ do
    cb <- asyncFunction $ handler triggerResizeE
    ro <- new (jsg "ResizeObserver") [cb]
    ro ^. js1 "observe" e
  return resizeE
  where
    handler triggerResizeE _ _ (entries : _) = do
      entry <- entries ^. js "0"
      cr <- entry ^. js "contentRect"
      x <- cr ^. js "x" >>= valToNumber
      y <- cr ^. js "y" >>= valToNumber
      w <- cr ^. js "width" >>= valToNumber
      h <- cr ^. js "height" >>= valToNumber
      t <- cr ^. js "top" >>= valToNumber
      r <- cr ^. js "right" >>= valToNumber
      b <- cr ^. js "bottom" >>= valToNumber
      l <- cr ^. js "left" >>= valToNumber
      triggerResizeE $ DomRect x y w h t r b l
    handler _ _ _ _ = pure ()
