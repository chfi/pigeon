module Sorting.BubbleSort where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log, logShow)
import Control.Monad.Except (Except, ExceptT(..), runExcept)
import Control.Monad.Except.Trans (class MonadError, except, runExceptT, throwError)
import Control.Monad.Free (Free, liftF, resume, runFreeM)
import Control.Monad.Free as Free
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.State (State, evalState, execState, get, runState)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.State.Trans (StateT(..), execStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), note)
import Data.Foldable (all, and, foldM, for_, length, or)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity(..))
import Data.Lazy (Lazy)
import Data.List (List, partition)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (mempty)
import Data.Ord (greaterThan)
import Data.Profunctor.Strong (fanout)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (singleton)
import Debug.Trace as Debug
import Partial.Unsafe (unsafePartial)
import Sorting (SortAlgorithm, peek, swap)
import Sorting as Sorting
import Unsafe.Coerce (unsafeCoerce)

{-
procedure bubbleSort( A : list of sortable items )
    n = length(A)
    repeat
        swapped = false
        for i = 1 to n-1 inclusive do
            /* if this pair is out of order */
            if A[i-1] > A[i] then
                /* swap them and remember something changed */
                swap( A[i-1], A[i] )
                swapped = true
            end if
        end for
    until not swapped
end procedure
-}


bubbleSort :: forall a.
              Ord a
           => Int
           -> SortAlgorithm a Unit
bubbleSort size =
  let step i = do
        let j = i + 1
        a <- peek i
        b <- peek j
        if a > b then swap i j *> pure true
                 else pure false

      bubble = do
        r <- or <$> traverse step (List.range 0 (size-2))
        if r then bubble
             else pure unit

  in bubble
