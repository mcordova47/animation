module Examples.Measuring
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Component.ReactRef (attachRef)
import Components.ReactUseMeasure (useMeasure')
import Components.ReactUseMeasure as ReactUseMeasure
import Data.Maybe (Maybe(..))
import Elmish (DispatchMsgFn, ReactElement, Transition)
import Elmish.HTML.Styled as S

type State = Unit

type Message = Void

init :: forall m. Transition m Message State
init =
  pure unit

view :: State -> DispatchMsgFn Message -> ReactElement
view state _ =
  S.div "text-center h-100" $
    useMeasure'
      { debounce: ReactUseMeasure.milliseconds 100.0
      , scroll: false
      }
      \ref bounds ->
        attachRef ref $
          S.div "h-100 w-100" $
            "Width: " <> show bounds.width <> "; Height: " <> show bounds.height
  where
    settings =
      Just
        { stiffness: Just 50.0
        , damping: Just 0.0
        , precision: Nothing
        }

update :: forall m. State -> Message -> Transition m Message State
update state _ =
  pure unit
