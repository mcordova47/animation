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
import Data.Maybe (Maybe(..))
import Elmish (DispatchMsgFn, ReactElement, Transition, handleMaybe)
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R
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
        R.fragment
          [ S.div_ ("position-relative" <> if state.modalVisible then "" else " cursor-pointer")
              { onClick: handleMaybe dispatch if state.modalVisible then Nothing else Just ToggleModal
              , style: containerStyle style
              }
              [ S.span_ "cursor-pointer position-absolute"
                  { onClick: handleMaybe dispatch if state.modalVisible then Just ToggleModal else Nothing
                  , style: iconStyle style
                  } $
                  octicon if state.modalVisible then "x" else "pencil"
              , if state.modalVisible then
                  S.div_ "py-2 px-4"
                    { style: contentStyle style }
                    [ S.h2 "" "Card Title"
                    , S.p "text-muted" "Some text"
                    ]
                else
                  R.empty
              ]
          ]
    , config: { tension: 300, friction: 30 }
    }
  where
    containerStyle s = S.css
      { boxShadow: s.containerBoxShadow
      , borderRadius: s.containerBorderRadius
      , background: s.containerBackground
      , height: s.containerHeight
      , width: s.containerWidth
      }
    iconStyle s = S.css
      { right: s.iconRight
      , transform: s.iconTransform
      }
    contentStyle s = S.css
      { opacity: s.contentOpacity
      }
    style' s =
      { containerBoxShadow
      , containerBorderRadius: switch "1em" "50%"
      , containerBackground: switch "linear-gradient(-45deg, white, white)" "linear-gradient(-45deg, #f6d365, #fda085)"
      , containerHeight: switch 200 50
      , containerWidth: switch 500 50
      , iconRight: switch "5em" "50%"
      , iconTransform: "translate(50%,50%)"
      , contentOpacity: switch 1.0 0.0
      }
      where
        switch :: forall a. a -> a -> a
        switch a b = if s.modalVisible then a else b
        containerBoxShadow = switch
          """
            0px 2px 1px -1px rgba(0, 0, 0, 0.2),
            0px 1px 1px 0px rgba(0, 0, 0, 0.14),
            0px 1px 3px 0px rgba(0,0,0,.12)
          """
          """
            0px 3px 5px -1px rgba(0, 0, 0, 0.2),
            0px 6px 10px 0px rgba(0, 0, 0, 0.14),
            0px 1px 18px 0px rgba(0,0,0,.12)
          """

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  ToggleModal ->
    pure state { modalVisible = not state.modalVisible }
