module Main
  ( main
  ) where

import Prelude

import Components.ReactMotion (interpolatingFunction, motion, spring)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Elmish (ComponentDef, boot, handle)
import Elmish.HTML as H
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R

main :: Effect Unit
main =
  boot
    { domElementId: "app"
    , def
    }

type State =
  { size :: Number
  }

data Message
  = ToggleSize

def :: forall m. ComponentDef m Message State
def =
  { init: pure { size: 0.0 }
  , update
  , view
  }
  where
    view state dispatch =
      R.fragment
        [ S.button_ "btn" { onClick: handle dispatch ToggleSize } "Toggle Visibility"
        , motion
            { defaultStyle: H.css { height: 0, width: 0 }
            , style: H.css { height: spring state.size Nothing, width: spring state.size Nothing }
            , render: interpolatingFunction \style ->
                S.div_ "box" { style } R.empty
            }
        ]
    update state = case _ of
      ToggleSize ->
        pure state { size = if state.size == 0.0 then 200.0 else 0.0 }
