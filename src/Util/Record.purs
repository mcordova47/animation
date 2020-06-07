module Util.Record
  ( class RowContainsAll
  , class RowSameLabels
  ) where

import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Cons, Nil)

class RowSameLabels (left :: #Type) (right :: #Type)
instance rowSameLabels ::
  ( RowToList left left'
  , RowToList right right'
  , RowContainsAll left' right
  , RowContainsAll right' left
  )
  => RowSameLabels left right

class RowContainsAll (left :: RowList) (right :: # Type)
instance rowListSameLabelsNil :: RowContainsAll Nil right
instance rowListSameLabelsCons ::
  ( Row.Cons name b rightTail right
  , RowContainsAll leftTail right
  )
  => RowContainsAll (Cons name a leftTail) right
