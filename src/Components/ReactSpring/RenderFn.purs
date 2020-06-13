module RenderFn
  ( RenderFn
  , renderFn
  ) where

import Elmish (ReactElement)
import Elmish.Foreign (class CanPassToJavaScript)

newtype RenderFn style = RenderFn ({ | style } -> ReactElement)
instance jsRenderFn :: CanPassToJavaScript { | style } => CanPassToJavaScript (RenderFn style)

renderFn :: forall style. ({ | style } -> ReactElement) -> RenderFn style
renderFn = RenderFn
