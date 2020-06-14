module Examples.ReactSpring.FloatingButton
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Components.Octicon (octicon)
import Components.ReactSpring.Spring (spring)
import Components.ReactSpring.Transition (item, keyFn, transition)
import Components.ReactSpring.Transition as T
import Elmish (DispatchMsgFn, ReactElement, Transition, handle)
import Elmish.HTML.Styled as S
import RenderFn (renderFn)

type State =
  { modalVisible :: Boolean
  }

data Message
  = ToggleModal

init :: forall m. Transition m Message State
init =
  pure initialState

initialState :: State
initialState =
  { modalVisible: false }

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  spring
    { from: style' initialState
    , to: style' state
    , render: renderFn \style ->
        S.div "text-center"
          [ S.div_ (if state.modalVisible then "pr-3" else "cursor-pointer d-flex align-items-center justify-content-center")
              { onClick: handle dispatch ToggleModal
              , style: S.css style
              } $
              transition
                { items: item state.modalVisible
                , keys: keyFn show
                , from: { opacity: 1.0 }
                , enter: { opacity: 1.0 }
                , leave: { opacity: 0.0 }
                , render: T.renderFn \item style'' ->
                    S.span_ (if item then "cursor-pointer float-right" else "d-inline-flex align-items-center justify-content-center float-right")
                      { style: S.css style'' } $
                      octicon if item then "x" else "pencil"
                , config: { tension: 400, friction: 20 }
                }
          ]
    , config: { tension: 200, friction: 30 }
    }
  where
    style' s =
      { boxShadow: boxShadow s
      , borderRadius: borderRadius s
      , background: background s
      , height: height s
      , width: width s
      }
    boxShadow s =
      if s.modalVisible then
        """
          0px 2px 1px -1px rgba(0, 0, 0, 0.2),
          0px 1px 1px 0px rgba(0, 0, 0, 0.14),
          0px 1px 3px 0px rgba(0,0,0,.12)
        """
      else
        """
          0px 3px 5px -1px rgba(0, 0, 0, 0.2),
          0px 6px 10px 0px rgba(0, 0, 0, 0.14),
          0px 1px 18px 0px rgba(0,0,0,.12)
        """
    borderRadius s =
      if s.modalVisible then "0" else "50%"
    background s =
      if s.modalVisible then "linear-gradient(-45deg, white, white)" else "linear-gradient(-45deg, #f6d365, #fda085)"
    height s =
      if s.modalVisible then 200 else 50
    width s =
      if s.modalVisible then 500 else 50

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  ToggleModal ->
    pure state { modalVisible = not state.modalVisible }
