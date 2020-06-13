module Examples.ReactSpring.Simple
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Components.ReactMotion (interpolatingFunction, motion, spring)
import Data.Maybe (Maybe(..))
import Elmish (DispatchMsgFn, ReactElement, Transition, handle)
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R

type State =
  { size :: Number
  }

data Message
  = ToggleSize

init :: forall m. Transition m Message State
init =
  pure { size: 0.0 }

type PlainStyle =
   { height :: Number
   , width :: Number
   }

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  S.div "text-center"
    [ S.button_ "btn btn-outline-primary" { onClick: handle dispatch ToggleSize } "Toggle Visibility"
    , motion
        { style: { height: spring state.size Nothing, width: spring state.size Nothing }
        , render: interpolatingFunction \(style :: PlainStyle) ->
            S.div_ "box rounded mt-2 mx-auto"
              { style: S.css style }
              R.empty
        }
    ]

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  ToggleSize ->
    pure state { size = if state.size == 0.0 then 200.0 else 0.0 }
