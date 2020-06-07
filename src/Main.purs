module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Elmish (ComponentDef, bimap, boot, handle, lmap, (>#<))
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R
import Examples.Oscillation as Oscillation
import Examples.Simple as Simple

main :: Effect Unit
main =
  boot
    { domElementId: "app"
    , def
    }

data Message
  = SwitchTab Tab
  | SimpleMsg Simple.Message
  | OscillationMsg Oscillation.Message

type State =
  { currentTab :: Tab
  , simple :: Simple.State
  , oscillation :: Oscillation.State
  }

data Tab
  = Simple
  | Oscillation
derive instance eqTab :: Eq Tab

displayTab :: Tab -> String
displayTab = case _ of
  Simple -> "Simple"
  Oscillation -> "Oscillation"

allTabs :: Array Tab
allTabs =
  [Simple, Oscillation]

def :: forall m. Monad m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init = do
      simple <- lmap SimpleMsg Simple.init
      oscillation <- lmap OscillationMsg Oscillation.init
      pure
        { currentTab: Simple
        , simple
        , oscillation
        }
    view state dispatch =
      R.fragment
        [ S.ul "nav nav-pills mt-3 ml-3" $
            allTabs <#> \tab ->
              S.li "nav-item" $
                S.button_ ("btn btn-link nav-link" <> if tab == state.currentTab then " active" else "")
                { onClick: handle dispatch $ SwitchTab tab } $
                  displayTab tab
        , S.div "row pt-4" $
            S.div "col-12 col-md-8 offset-md-2 col-lg-6 offset-lg-3"
              case state.currentTab of
                Simple ->
                  Simple.view state.simple (dispatch >#< SimpleMsg)
                Oscillation ->
                  Oscillation.view state.oscillation (dispatch >#< OscillationMsg)
        ]
    update state = case _ of
      SwitchTab tab ->
        pure state { currentTab = tab }
      SimpleMsg msg ->
        bimap SimpleMsg (state { simple = _ }) $ Simple.update state.simple msg
      OscillationMsg msg ->
        bimap OscillationMsg (state { oscillation = _ }) $ Oscillation.update state.oscillation msg
