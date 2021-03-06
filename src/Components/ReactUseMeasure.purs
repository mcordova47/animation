module Components.ReactUseMeasure
  ( DebounceConfig
  , Options
  , RectReadOnly
  , milliseconds
  , useMeasure
  , useMeasure'
  ) where

import Prelude

import Component.ReactRef (ReactRef)
import Data.Function.Uncurried (Fn2, mkFn2)
import Elmish (ReactElement, createElement')
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.React.Import (ImportedReactComponent)
import Foreign (Foreign, unsafeToForeign)
import Record as Record

type RectReadOnly =
  { x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , top :: Number
  , right :: Number
  , bottom :: Number
  , left :: Number
  }

type Options =
  { debounce :: DebounceConfig
  , scroll :: Boolean
  }

newtype DebounceConfig = DebounceConfig Foreign
instance jsDebounceConfig :: CanPassToJavaScript DebounceConfig

milliseconds :: Number -> DebounceConfig
milliseconds =
  DebounceConfig <<< unsafeToForeign

newtype RenderFn = RenderFn (Fn2 ReactRef RectReadOnly ReactElement)
instance jsRenderFn :: CanPassToJavaScript RenderFn

useMeasure :: (ReactRef -> RectReadOnly -> ReactElement) -> ReactElement
useMeasure render =
  createElement' useMeasure_ { render: RenderFn $ mkFn2 render }

useMeasure' :: Options -> (ReactRef -> RectReadOnly -> ReactElement) -> ReactElement
useMeasure' options render =
  createElement' useMeasure_ $ Record.merge options { render: RenderFn $ mkFn2 render }

foreign import useMeasure_ :: ImportedReactComponent
