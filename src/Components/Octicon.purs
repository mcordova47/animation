module Components.Octicon
  ( octicon
  ) where

import Data.Function.Uncurried (Fn1, runFn1)
import Elmish (ReactElement, createElement')
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.React.Import (EmptyProps, ImportedReactComponentConstructor, ImportedReactComponent)

octicon :: String -> ReactElement
octicon name =
  octicon' { icon: getIconByName name }

type Props r =
  ( icon :: Icon
  | r
  )

octicon' :: ImportedReactComponentConstructor Props EmptyProps
octicon' =
  createElement' octicon_

getIconByName :: String -> Icon
getIconByName =
  runFn1 getIconByName_

foreign import data Icon :: Type
instance jsIcon :: CanPassToJavaScript Icon

foreign import octicon_ :: ImportedReactComponent
foreign import getIconByName_ :: Fn1 String Icon
