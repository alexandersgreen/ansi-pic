module Lib
    ( ansiImage
    ) where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Data.Colour.SRGB
import Data.Foldable
import System.Console.ANSI
import System.Console.Terminal.Size

loadImage :: FilePath -> IO (Image PixelRGB8)
loadImage file = do
  res <- readImage file
  either error (pure . convertRGB8) res

scaleImage :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImage maxW maxH img = scaleBilinear w h img
  where
    (w,h) = scaleDimensions (maxW `div` 2) maxH (imageWidth img) (imageHeight img)

scaleDimensions :: Int -> Int -> Int -> Int -> (Int,Int)
scaleDimensions maxW maxH inW inH
  | inW <= maxW
  , inH <= maxH
  = (inW,inH)
  | wFactor <- inW `div` maxW
  , scaledH <- inH `div` wFactor
  , scaledH <= maxH
  = (maxW,scaledH)
  | otherwise
  = (inW `div` (inH `div` maxH),maxH)

printImage :: Image PixelRGB8 -> IO ()
printImage img =
  for_ [1..imageHeight img] $ \h -> do
    for_ [1..imageWidth img] $ \w -> do
      let (PixelRGBF r g b) = promotePixel $ pixelAt img (w-1) (h-1)
      setSGR [SetRGBColor Background $ sRGB r g b]
      putStr "  "
    setSGR [Reset]
    putStrLn ""

ansiImage :: FilePath -> IO ()
ansiImage file = do
  mSize <- size
  case mSize of
    Nothing -> error "Can't determine terminal size"
    Just (Window h w) -> do
      img <- scaleImage w (h-2) <$> loadImage file
      printImage img
