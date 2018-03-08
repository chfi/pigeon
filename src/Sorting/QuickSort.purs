module Sorting.QuickSort where

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
algorithm quicksort(A, lo, hi) is
    if lo < hi then
        p := partition(A, lo, hi)
        quicksort(A, lo, p - 1 )
        quicksort(A, p + 1, hi)

algorithm partition(A, lo, hi) is
    pivot := A[hi]
    i := lo - 1
    for j := lo to hi - 1 do
        if A[j] < pivot then
            i := i + 1
            swap A[i] with A[j]
    swap A[i + 1] with A[hi]
    return i + 1
-}


quickSort :: forall x.
             Ord x
          => Show x
          => Int
          -> SortAlgorithm x Unit
quickSort size =
  let part' :: Int -> Int -> SortAlgorithm x Int
      part' lo hi = do

        pivot <- peek hi

        let loop' :: Int -> Int -> SortAlgorithm x Int
            loop' i j = do
              a <- peek j
              if a < pivot then swap i j *> pure (i+1)
                           else pure i

        -- i' <- foldM loop' lo $ List.range lo hi
        i' <- Sorting.for' (Tuple lo hi) loop' lo
        swap i' hi
        pure i'


      qs' :: Int -> Int -> SortAlgorithm x Unit
      qs' lo hi
        | lo < hi = do
            p <- part' lo hi
            qs' lo   (p-1)
            qs' (p+1) hi
        | otherwise = pure unit

  in qs' 0 (size-1)
