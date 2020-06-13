module Components.ReactSpring.Config
  ( Config
  , default
  , gentle
  , wobbly
  , stiff
  , slow
  , molasses
  ) where

type Config =
  { tension :: Int
  , friction :: Int
  }

foreign import default :: Config
foreign import gentle :: Config
foreign import wobbly :: Config
foreign import stiff :: Config
foreign import slow :: Config
foreign import molasses :: Config
