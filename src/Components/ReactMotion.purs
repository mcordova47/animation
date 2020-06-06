module Components.ReactMotion
  ( InterpolatingFunction
  , OpaqueConfig
  , Props
  , SpringHelperConfig
  , SpringHelperConfig'
  , interpolatingFunction
  , motion
  , spring
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Elmish (ReactElement, createElement')
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.HTML (CSS)
import Elmish.React.Import (EmptyProps, ImportedReactComponent, ImportedReactComponentConstructor)

type Props r =
  ( render :: InterpolatingFunction
  , style :: CSS
  , defaultStyle :: CSS
  | r
  )

newtype InterpolatingFunction = InterpolatingFunction (CSS -> ReactElement)
instance jsInterpolatingFunction :: CanPassToJavaScript InterpolatingFunction

interpolatingFunction :: (CSS -> ReactElement) -> InterpolatingFunction
interpolatingFunction = InterpolatingFunction

motion :: ImportedReactComponentConstructor Props EmptyProps
motion = createElement' motion_

type SpringHelperConfig = SpringHelperConfig' Maybe
type SpringHelperConfig' f =
  { stiffness :: f Number
  , damping :: f Number
  , precision :: f Number
  }

spring :: Number -> Maybe SpringHelperConfig -> OpaqueConfig
spring val config =
  runFn2 spring_ val $ toNullable $ jsConfig <$> config
  where
    jsConfig c =
      { stiffness: toNullable c.stiffness
      , damping: toNullable c.damping
      , precision: toNullable c.precision
      }

foreign import motion_ :: ImportedReactComponent
foreign import data OpaqueConfig :: Type
foreign import spring_ :: Fn2 Number (Nullable (SpringHelperConfig' Nullable)) OpaqueConfig
