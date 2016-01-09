{-# LANGUAGE TemplateHaskell #-}
module Reflex.Dom.LazyGrid.Css where

import Data.ByteString
import Data.FileEmbed (embedFile)

lazyGridCss :: ByteString
lazyGridCss = $(embedFile "grid.css")
