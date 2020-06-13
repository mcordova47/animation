module Components.ReactSpring.Transition
  ( Items
  , KeyFn
  , Props
  , RenderFn
  , item
  , items
  , keyFn
  , renderFn
  , transition
  ) where

import Prelude

import Components.ReactSpring.Config (Config)
import Elmish (ReactElement, createElement')
import Elmish.Foreign (class CanPassToJavaScript, Foreign)
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructor)
import Foreign (unsafeToForeign)

type Props item style r =
  ( items :: Items item
  , keys :: KeyFn item
  , from :: { | style }
  , enter :: { | style }
  , leave :: { | style }
  , render :: RenderFn item style
  | r
  )

newtype Items item = Items Foreign
instance jsItems :: CanPassToJavaScript (Items items)

items :: forall item. Array item -> Items item
items =
  Items <<< unsafeToForeign

item :: forall item. item -> Items item
item =
  Items <<< unsafeToForeign

newtype KeyFn item = KeyFn (item -> String)
instance jsKeyFn :: CanPassToJavaScript (KeyFn item)

keyFn :: forall item. (item -> String) -> KeyFn item
keyFn = KeyFn

newtype RenderFn item style = RenderFn (item -> { | style } -> ReactElement)
instance jsRenderFn :: CanPassToJavaScript { | style } => CanPassToJavaScript (RenderFn item style)

renderFn :: forall item style. (item -> { | style } -> ReactElement) -> RenderFn item style
renderFn =
  RenderFn

type OptProps r =
  ( config :: Config
  | r
  )

transition :: forall item style. ImportedReactComponentConstructor (Props item style) OptProps
transition =
  createElement' transition_

foreign import transition_ :: ImportedReactComponent
