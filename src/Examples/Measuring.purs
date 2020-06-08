module Examples.Measuring
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Components.ReactUseMeasure (useMeasure')
import Components.ReactUseMeasure as ReactUseMeasure
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
      { debounce: ReactUseMeasure.milliseconds 50.0
      , scroll: false
      }
      \bounds ->
        S.div "h-100 w-100" $
          "Width: " <> show bounds.width <> "; Height: " <> show bounds.height

update :: forall m. State -> Message -> Transition m Message State
update state _ =
  pure unit
