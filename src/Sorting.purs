module Sorting
       ( SortError(..)
       , SortErrors
       , SortStepF(..)
       , SortAlgorithm
       , peek
       , swap
       , execAlgorithm
       , iterSort
       , stepper
       , for'
       , peek'
       , set'
       ) where

import Prelude

import Control.Monad.Except (Except, runExcept)
import Control.Monad.Except.Trans (class MonadError, throwError)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.State.Trans (StateT, execStateT, runStateT)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafePartial)


data SortError
  = OutOfBounds { size :: Int, attemptedIndex :: Int }

derive instance genericSortError :: Generic SortError _

instance showSortError :: Show SortError where
  show x = genericShow x

type SortErrors = NonEmptyList SortError

data SortStepF x a
  = Peek Int (x -> a)
  | Swap Int Int a

derive instance functorSortStepF :: Functor (SortStepF x)

type SortAlgorithm x = Free (SortStepF x)


peek :: forall x. Int -> SortAlgorithm x x
peek i = liftF $ Peek i id

swap :: forall x. Int -> Int -> SortAlgorithm x Unit
swap i j = liftF $ Swap i j unit

for' :: forall x a.
        (Tuple Int Int)
     -> (a -> Int -> SortAlgorithm x a)
     -> a
     -> SortAlgorithm x a
for' (Tuple l h) f init =
  foldM f init (List.range l h)


safeIndex :: forall m a.
             MonadError SortErrors m
          => List a
          -> Int
          -> m Int
safeIndex list i =
  if i < 0 || i >= List.length list
     then do
       let size = List.length list
       throwError $ NE.singleton
                  $ OutOfBounds { size, attemptedIndex: i }
     else pure i


peek' :: forall m x.
         MonadError SortErrors m
      => MonadState (List x)   m
      => Int
      -> m x
peek' i' = unsafePartial do
  list <- get
  i <- safeIndex list i'
  pure $ fromJust $ List.index list i

set' :: forall m x a.
        MonadError SortErrors m
     => MonadState (List x)   m
     => Int
     -> x
     -> m Unit
set' i' x = unsafePartial do
  list <- get
  i <- safeIndex list i'
  put $ fromJust $ List.updateAt i x list


execSortStep :: forall m x a.
                MonadRec m
             => MonadState (List x)   m
             => MonadError SortErrors m
             => SortStepF x a
             -> m a
execSortStep (Peek i cont) = do
  a <- peek' i
  pure (cont a)
execSortStep (Swap i j cont) = do
  a <- peek' i
  b <- peek' j
  set' i b
  set' j a
  pure cont


type SortStack x a = StateT (List x) (Except SortErrors) a


execAlgorithm :: forall x a.
                 Ord x
              => SortAlgorithm x a
              -> List x
              -> Either SortErrors (List x)
execAlgorithm algorithm list =
  runExcept $ execStateT (runFreeM execSortStep algorithm) list


iterSort :: forall x a.
            Ord x
         => Either SortErrors (Tuple (Maybe (SortAlgorithm x a)) (List x))
         -> Either SortErrors (Tuple (Maybe (SortAlgorithm x a)) (List x))
iterSort e@(Left _) = e
iterSort  (Right t) = stepSort t
  where stepSort r@(Tuple Nothing  _) = pure r
        stepSort (Tuple (Just alg) list) =
          case Free.resume alg of
            Right _ -> pure $ Tuple Nothing list
            Left f' -> map (lmap Just)
                       $ runExcept $ runStateT (execSortStep f') list


stepper :: forall x a.
            Ord x
         => SortAlgorithm x a
         -> List x
         -> Either SortErrors (Tuple (Maybe (SortAlgorithm x a)) (List x))
stepper alg list = iterSort (Right (Tuple (Just alg) list))
