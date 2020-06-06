module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef, boot)
-- import Elmish.HTML.Styled as H
import Elmish.React.DOM as R

main :: Effect Unit
main =
  boot
    { domElementId: "app"
    , def
    }

type State = Unit

type Message = Void

def :: forall m. ComponentDef m Message State
def =
  { init: pure unit
  , update: \_ _ -> pure unit
  , view
  }
  where
    view _ _ =
      R.empty
