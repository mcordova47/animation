module Examples.Oscillation
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Components.ReactMotion (interpolatingFunction, motion, spring)
import Data.Maybe (Maybe(..))
import Elmish (DispatchMsgFn, ReactElement, Transition)
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R

type State = Unit

type Message = Void

init :: forall m. Transition m Message State
init =
  pure unit

view :: State -> DispatchMsgFn Message -> ReactElement
view state _ =
  S.div "text-center" $
    motion
      { defaultStyle: { left: 0.0 }
      , style: { left: spring 42.0 settings }
      , render: interpolatingFunction \style ->
          S.div "oscillating-box-track position-relative rounded mx-auto" $
            S.div_ "oscillating-box rounded mx-auto position-absolute"
              { style: S.css { left: show style.left <> "%" } }
              R.empty
      }
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
