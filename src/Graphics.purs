module Graphics where

import Prelude

import Control.Monad.Writer (Writer)
import Control.Monad.Writer.Trans (tell)
import Data.Foldable (fold)
import Data.Int as Int
import Data.List (List)
import Data.List as List
import Data.Monoid (mempty)
import Data.Tuple (uncurry)
import Graphics.Canvas as Canvas
import Graphics.Drawing (Color, Drawing, Shape, fillColor, filled, outlineColor, outlined, rectangle)
import Graphics.Drawing as Drawing
import Sorting (SortStepF(..))


type Dimensions = { colWidth :: Number
                  , colPad :: Number
                  , colHeight :: Number
                  , canvas :: Canvas.Dimensions
                  , padding :: { h :: Number, v :: Number }
                  }

dimensionsFromCanvas :: Canvas.Dimensions
                     -> { canvasRatio :: { width :: Number, height :: Number }
                        , columnsRatio :: { widthToPad :: Number, heightToCanvas :: Number } }
                     -> Int
                     -> Dimensions
dimensionsFromCanvas cd { canvasRatio, columnsRatio } n =
  let n' = Int.toNumber n
      width  = cd.width  * canvasRatio.width
      height = cd.height * canvasRatio.height

      colSpace  = width / n'
      colWidth  = columnsRatio.widthToPad * colSpace

      colPad    = (1.0 - columnsRatio.widthToPad) * colSpace

      colHeight = columnsRatio.heightToCanvas * cd.height

      padding = { h: (cd.width  * (1.0 - canvasRatio.width))  / 2.0
                , v: (cd.height * (1.0 - canvasRatio.height)) / 2.0 }

  in { colWidth, colPad, colHeight, canvas: { width, height }, padding }


type StepColors = { inactive :: Color
                  , peeking  :: Color
                  , swapping :: Color }


column :: forall a r.
          { colWidth :: Number | r }
       -> (a -> Number)
       -> a
       -> Shape
column { colWidth } f a =
  let h = f a
      w = colWidth
      r = w / 2.0
  in rectangle (-r) zero w h


basicStyle :: Color -> Shape -> Drawing
basicStyle c s = outlined (outlineColor c) s
              <> filled (fillColor c) s


shapes' :: forall m x a.
           Dimensions
        -> Number
        -> List Number
        -> List Shape
shapes' dims y ys =
  let f i a = rectangle (mkX i)       (mkY a)
                        dims.colWidth (mkH a)
      mkX i = dims.padding.h + (Int.toNumber i * (dims.colPad + dims.colWidth))
      mkH a = dims.colHeight * (a / y)
      mkY a = dims.canvas.height - dims.padding.v - (mkH a)

  in map (uncurry f) $ List.zip (List.range zero (List.length ys)) ys



colorStep :: forall x a.
             StepColors
          -> Int
          -> SortStepF x a
          -> List Color
colorStep colors size =
  let cs g t = map (\i -> if g i then t else colors.inactive)
                   $ List.range zero (size)
  in case _ of
    Peek i _   -> cs (_ == i) colors.peeking
    Swap i j _ -> cs (\i' -> i' == i || i' == j) colors.swapping


drawStep :: forall m x a.
            StepColors
         -> List Shape
         -> SortStepF Shape a
         -> Writer Drawing a
drawStep colors shapes p@(Peek i next) = do
  let cs = colorStep colors (List.length shapes) p
  tell $ fold $ List.zipWith basicStyle cs shapes
  pure $ next mempty
drawStep colors shapes s@(Swap i j next) = do
  let cs = colorStep colors (List.length shapes) s
  tell $ fold $ List.zipWith basicStyle cs shapes
  pure next
