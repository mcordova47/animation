module Main
  ( main
  ) where

import Prelude

import Components.ReactSpring.Transition (item, keyFn, renderFn, transition)
import Data.Array (groupBy)
import Data.Array.NonEmpty as NE
import Data.Function (on)
import Effect (Effect)
import Elmish (ComponentDef, bimap, boot, handle, lmap, (>#<))
import Elmish.HTML.Styled as S
import Elmish.React.DOM as R
import Examples.Collapsing as Collapsing
import Examples.Oscillation as Oscillation
import Examples.ReactSpring.FloatingButton as FloatingButton
import Examples.ReactSpring.Simple as SpringSimple
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
  | CollapsingMsg Collapsing.Message
  | STSimpleMsg SpringSimple.Message
  | FloatingButtonMsg FloatingButton.Message

type State =
  { currentTab :: Tab
  , simple :: Simple.State
  , oscillation :: Oscillation.State
  , collapsing :: Collapsing.State
  , stSimple :: SpringSimple.State
  , floatingButton :: FloatingButton.State
  }

data Tab
  = Motion MotionTab
  | Spring SpringTab
derive instance eqTab :: Eq Tab

data MotionTab
  = Simple
  | Oscillation
  | Collapsing
derive instance eqMotionTab :: Eq MotionTab

data SpringTab
  = STSimple
  | FloatingButton
derive instance eqSpringTab :: Eq SpringTab

tabLabel :: Tab -> String
tabLabel = case _ of
  Motion Simple -> "Simple"
  Motion Oscillation -> "Oscillation"
  Motion Collapsing -> "Collapsing"
  Spring STSimple -> "Simple"
  Spring FloatingButton -> "Floating Button"

tabCategory :: Tab -> String
tabCategory = case _ of
  Motion _ -> "React Motion"
  Spring _ -> "React Spring"

tabKey :: Tab -> String
tabKey = case _ of
  Motion Simple -> "simple"
  Motion Oscillation -> "oscillation"
  Motion Collapsing -> "collapsing"
  Spring STSimple -> "st-simple"
  Spring FloatingButton -> "floating-button"

allTabs :: Array Tab
allTabs =
  [ Motion Simple
  , Motion Oscillation
  , Motion Collapsing
  , Spring STSimple
  , Spring FloatingButton
  ]

def :: forall m. Monad m => ComponentDef m Message State
def =
  { init, update, view }
  where
    init = do
      simple <- lmap SimpleMsg Simple.init
      oscillation <- lmap OscillationMsg Oscillation.init
      collapsing <- lmap CollapsingMsg Collapsing.init
      stSimple <- lmap STSimpleMsg SpringSimple.init
      floatingButton <- lmap FloatingButtonMsg FloatingButton.init
      pure
        { currentTab: Motion Simple
        , simple
        , oscillation
        , collapsing
        , stSimple
        , floatingButton
        }
    view state dispatch =
      R.fragment
        [ S.div "main-nav position-absolute p-3" $
            S.div "card border-0" $
              S.div "card-body bg-light rounded" $
                allTabs # groupBy ((==) `on` tabCategory) <#> renderCategory
        , S.div "row pt-4 vh-100" $
            transition
              { items: item state.currentTab
              , keys: keyFn tabKey
              , from: { opacity: 0.0, transform: "translate3d(100%,0,0)" }
              , enter: { opacity: 1.0, transform: "translate3d(0%,0,0)" }
              , leave: { opacity: 0.0, transform: "translate3d(-50%,0,0)" }
              , render: renderFn \item style ->
                  S.div_ "col-12 col-md-8 offset-md-2 col-lg-6 offset-lg-3 position-absolute"
                    { style: S.css style }
                    case item of
                      Motion Simple ->
                        Simple.view state.simple (dispatch >#< SimpleMsg)
                      Motion Oscillation ->
                        Oscillation.view state.oscillation (dispatch >#< OscillationMsg)
                      Motion Collapsing ->
                        Collapsing.view state.collapsing (dispatch >#< CollapsingMsg)
                      Spring STSimple ->
                        SpringSimple.view state.stSimple (dispatch >#< STSimpleMsg)
                      Spring FloatingButton ->
                        FloatingButton.view state.floatingButton (dispatch >#< FloatingButtonMsg)
              }
        ]
      where
        renderCategory tabs | category <- tabCategory (NE.head tabs) = R.fragment $
          [ S.h6 "" category
          , S.ul "nav nav-pills flex-column pt-1 pb-3" $
              NE.toArray tabs <#> \tab ->
                S.li "nav-item" $
                  S.button_ ("btn btn-link nav-link" <> if tab == state.currentTab then " active" else "")
                  { onClick: handle dispatch $ SwitchTab tab } $
                    tabLabel tab
          ]
    update state = case _ of
      SwitchTab tab ->
        pure state { currentTab = tab }
      SimpleMsg msg ->
        bimap SimpleMsg (state { simple = _ }) $ Simple.update state.simple msg
      OscillationMsg msg ->
        bimap OscillationMsg (state { oscillation = _ }) $ Oscillation.update state.oscillation msg
      CollapsingMsg msg ->
        bimap CollapsingMsg (state { collapsing = _ }) $ Collapsing.update state.collapsing msg
      STSimpleMsg msg ->
        bimap STSimpleMsg (state { stSimple = _ }) $ SpringSimple.update state.stSimple msg
      FloatingButtonMsg msg ->
        bimap FloatingButtonMsg (state { floatingButton = _ }) $ FloatingButton.update state.floatingButton msg
