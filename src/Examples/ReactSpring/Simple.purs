module Examples.ReactSpring.Simple
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Components.ReactSpring.Config as Config
import Components.ReactSpring.Spring (spring)
import Elmish (DispatchMsgFn, ReactElement, Transition, handle)
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R
import RenderFn (renderFn)

type State =
  { visible :: Boolean
  }

data Message
  = ToggleVisibility

init :: forall m. Transition m Message State
init =
  pure initialState

initialState :: State
initialState =
  { visible: false }

size :: State -> Number
size state =
  if state.visible then 200.0 else 0.0

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  S.div "text-center"
    [ S.button_ "btn btn-outline-primary"
        { onClick: handle dispatch ToggleVisibility }
        "Toggle Visibility"
    , spring
        { from: { height: size initialState, width: size initialState }
        , to: { height: size state, width: size state }
        , render: renderFn \style ->
            S.div_ "box rounded mt-2 mx-auto"
              { style: S.css style }
              R.empty
        , config: if state.visible then Config.wobbly else Config.gentle
        }
    ]

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  ToggleVisibility ->
    pure state { visible = not state.visible }
