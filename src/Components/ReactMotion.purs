module Components.ReactMotion
  ( InterpolatingFunction
  , OpaqueConfig
  , Props
  , SpringHelperConfig
  , SpringHelperConfig'
  , interpolatingFunction
  , jump
  , motion
  , spring
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Elmish (ReactElement, createElement')
import Elmish.Foreign (class CanPassToJavaScript, Foreign)
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructor)
import Foreign (unsafeToForeign)
import Type.Row.Homogeneous (class Homogeneous)
import Util.Record (class RowSameLabels)

type Props style plainStyle r =
  ( render :: InterpolatingFunction plainStyle
  , style :: style
  | r
  )

type OptProps plainStyle r =
  ( defaultStyle :: plainStyle
  | r
  )

newtype InterpolatingFunction style = InterpolatingFunction (style -> ReactElement)
instance jsInterpolatingFunction :: CanPassToJavaScript (InterpolatingFunction a)

interpolatingFunction :: forall plainStyle
   . Homogeneous plainStyle Number
  => ({ | plainStyle } -> ReactElement)
  -> InterpolatingFunction { | plainStyle }
interpolatingFunction =
  InterpolatingFunction

motion :: forall style plainStyle
   . Homogeneous style OpaqueConfig
  => Homogeneous plainStyle Number
  => RowSameLabels style plainStyle
  => ImportedReactComponentConstructor (Props { | style } { | plainStyle }) (OptProps { | plainStyle })
motion =
  createElement' motion_

type SpringHelperConfig = SpringHelperConfig' Maybe
type SpringHelperConfig' f =
  { stiffness :: f Number
  , damping :: f Number
  , precision :: f Number
  }

newtype OpaqueConfig = OpaqueConfig Foreign
instance jsOpaqueConfig :: CanPassToJavaScript OpaqueConfig

spring :: Number -> Maybe SpringHelperConfig -> OpaqueConfig
spring val config =
  OpaqueConfig $ runFn2 spring_ val $ toNullable $ jsConfig <$> config
  where
    jsConfig c =
      { stiffness: toNullable c.stiffness
      , damping: toNullable c.damping
      , precision: toNullable c.precision
      }

jump :: Number -> OpaqueConfig
jump =
  OpaqueConfig <<< unsafeToForeign

foreign import motion_ :: ImportedReactComponent
foreign import spring_ :: Fn2 Number (Nullable (SpringHelperConfig' Nullable)) Foreign
