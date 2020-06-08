module Examples.Collapsing
  ( Message
  , State
  , init
  , update
  , view
  ) where

import Prelude

import Component.ReactRef (attachRef)
import Components.Octicon (octicon)
import Components.ReactMotion (interpolatingFunction, motion, spring)
import Components.ReactUseMeasure (useMeasure')
import Components.ReactUseMeasure as ReactUseMeasure
import Data.Maybe (Maybe(..))
import Elmish (DispatchMsgFn, ReactElement, Transition, handle)
import Elmish.HTML.Styled as S

type State =
  { collapsed :: Boolean
  }

data Message
  = ToggleCollapse

init :: forall m. Transition m Message State
init =
  pure { collapsed: true }

view :: State -> DispatchMsgFn Message -> ReactElement
view state dispatch =
  S.div "text-center h-100 bg-light rounded py-3 px-5" $
    S.div "bg-white rounded p-2 mx-5"
      [ S.div_ "row cursor-pointer"
          { onClick: handle dispatch ToggleCollapse }
          [ S.div "col" $
              S.h6 "text-uppercase m-0" if state.collapsed then "Expand" else "Collapse"
          , S.div "col-auto" $
              motion
                { style: { rotation: spring (if state.collapsed then 0.0 else -180.0) settings }
                , render: interpolatingFunction \(style :: { rotation :: Number }) ->
                    S.span_ "float-right"
                      { style: S.css { transform: "rotate(" <> show style.rotation <> "deg)" } } $
                      octicon "chevron-down"
                }
          ]
      , useMeasure'
          { debounce: ReactUseMeasure.milliseconds 50.0
          , scroll: false
          }
          \ref bounds ->
            motion
              { style: { height: spring (if state.collapsed then 0.0 else bounds.height) settings }
              , render: interpolatingFunction \(style :: { height :: Number }) ->
                  S.div_ "overflow-hidden"
                    { style: if style.height == bounds.height then S.css { height: "auto" } else S.css style } $
                    attachRef ref $
                      S.div "" $
                        [ S.div "p-3" $
                            S.hr "m-0"
                        , S.p "" """
                              Maiores veritatis est aliquam. Quos iusto rem ut eveniet.
                              Cum voluptatibus et labore nisi dolorem velit libero.
                              Libero et possimus quos ut quia aut earum quia. Ab qui ex
                              esse quaerat itaque voluptate aliquam voluptas.
                            """
                        , S.p "" """
                              Distinctio commodi fugit ex est repellendus libero consequatur.
                              Quis officiis alias doloribus officiis velit debitis officia.
                              Incidunt impedit sunt voluptatem minima.
                            """
                        , S.p "" """
                              Doloribus et delectus consequatur. Assumenda laborum rem aut
                              veniam dignissimos eum corrupti magni. Commodi quia ad maiores
                              doloremque nihil unde. Explicabo quam enim illo laborum.
                              Repellendus saepe soluta est possimus necessitatibus.
                              Quo autem alias quibusdam et corrupti temporibus voluptate.
                            """
                        , S.p "" """
                              Quidem fuga et reiciendis voluptas aut perspiciatis magni ut.
                              Et ex enim nihil consequatur in. Error id delectus autem iure
                              velit aperiam. Eos et sed eum earum vero voluptatibus. Ea
                              consectetur ut et harum esse consequuntur sit consequuntur.
                              Porro placeat commodi quaerat et voluptas animi delectus.
                            """
                        , S.p "m-0 pb-2" """
                              Ut sapiente consequatur et sed praesentium quidem sequi.
                              Voluptatibus molestias voluptatem velit id sed incidunt.
                              Possimus nulla praesentium accusantium ut sint aut ex eos.
                              Qui nihil tempora dolore exercitationem beatae sed neque eligendi.
                              Eveniet maiores a possimus. Est rerum est molestiae officiis
                              repellendus suscipit.
                          """
                        ]
              }
      ]
  where
    settings = Just
      { stiffness: Just 300.0
      , damping: Just 35.0
      , precision: Just 0.01
      }

update :: forall m. State -> Message -> Transition m Message State
update state = case _ of
  ToggleCollapse ->
    pure state { collapsed = not state.collapsed }
