module Component.ReactRef
  ( ReactRef
  , attachRef
  ) where

import Elmish (ReactElement)

-- | A React `ref` object (or function), suitable for passing to the `ref` prop
-- | on DOM nodes. These values may be obtained from a `React.createRef` call or
-- | passed in from other React native components.
foreign import data ReactRef :: Type

-- | Clones the given node while adding a `ref` prop to it with the given ref
-- | value. Except when the component already has a `ref` attached to it, in
-- | which case this function will do nothing. Or at least that's how I
-- | understood React docs.
foreign import attachRef :: ReactRef -> ReactElement -> ReactElement
