module Main where

import Prelude

import Color (rgb, white)
import Color.Scheme.Clrs (purple, red)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Eff.Random as Eff
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Foldable (foldMap)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(Just, Nothing))
import Data.Traversable (traverse)
import Graphics (basicStyle, dimensionsFromCanvas, shapes')
import Graphics.Canvas as Canvas
import Graphics.Drawing (fillColor, filled, rectangle)
import Graphics.Drawing as Drawing
import Sorting (execAlgorithm)
import Sorting.QuickSort (quickSort)


tests :: List Int
tests = List.fromFoldable
  [ 5, 1, 9, 12431, 22, 4, 999 ]


genList :: Int -> Eff _ (List Number)
genList n = traverse (const Eff.random) (List.range 1 n)

foreign import setWindow :: forall a eff.
                            String -> a -> Eff eff Unit

main :: Eff _ Unit
main = do

  let n = 40

  list <- genList n

  canvas <- do
    c <- Canvas.getCanvasElementById "canvas"
    case c of
      Nothing -> throw "Couldn't find canvas!"
      Just c' -> pure c'

  ctx <- Canvas.getContext2D canvas

  cd@{width, height} <- Canvas.getCanvasDimensions canvas


  let colors = { inactive: white
               , peeking:  purple
               , swapping: red }

      dims = dimensionsFromCanvas cd
               { canvasRatio:  { width: 0.95, height: 0.9 }
               , columnsRatio: { widthToPad: 0.9
                               , heightToCanvas: 0.9 }
               } n


  let drawings = foldMap (basicStyle red) $ shapes' dims one list
      bg = filled (fillColor (rgb 20 20 20)) $ rectangle zero zero width height

  Drawing.render ctx $ bg <> drawings


  log "sorted tests: "
  logShow
    $ rmap Array.fromFoldable
    $ execAlgorithm (quickSort $ List.length tests) tests
