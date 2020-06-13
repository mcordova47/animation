module Components.ReactSpring.Spring
  ( Props
  , spring
  ) where

import Components.ReactSpring.Config (Config)
import Elmish (createElement')
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructor)
import RenderFn (RenderFn)

type Props style r =
  ( from :: { | style }
  , to :: { | style }
  , render :: RenderFn style
  | r
  )

type OptProps r =
  ( config :: Config
  | r
  )

spring :: forall style. ImportedReactComponentConstructor (Props style) OptProps
spring =
  createElement' spring_

foreign import spring_ :: ImportedReactComponent
