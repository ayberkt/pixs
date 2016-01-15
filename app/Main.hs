{-# LANGUAGE UnicodeSyntax #-}

import           Data.Either                (partitionEithers)
import           Codec.Picture              (DynamicImage (..), Image,
                                             PixelRGBA8, readImage, writePng)
-- import qualified Pixs.Filter                as F
-- import qualified Pixs.Information.Histogram as H
import qualified Pixs.Transformation        as T
import qualified Pixs.Arithmetic            as Arith
import           Prelude                    hiding (error, flip, and, or)
import qualified Options.Applicative        as A
import           Options.Applicative        (Parser, (<>))

data CommandType =
    SingleT FilePath   FilePath          (Image PixelRGBA8 → Image PixelRGBA8)
  | MultiT  [FilePath] FilePath          ([Image PixelRGBA8] → Image PixelRGBA8)
  | ArgT    FilePath   Int      FilePath (Int → Image PixelRGBA8 → Image PixelRGBA8)

inputOption ∷ Parser FilePath
inputOption = A.strOption (   A.long "in"
                           <> A.metavar "INPUT"
                           <> A.help "Input image file to be transformed")

outputOption ∷ Parser FilePath
outputOption = A.strOption (   A.long "out"
                            <> A.metavar "OUTPUT"
                            <> A.help "File name to write to.")

magnitudeOption ∷ Parser Int
magnitudeOption = (A.option A.auto
                   (   A.long "magnitude"
                    <> A.short 'm'
                    <> A.metavar "MAGNITUDE"
                    <> A.help "Magnitude of change"))

brightness ∷ Parser CommandType
brightness = ArgT
    <$> inputOption
    <*> (A.option A.auto
           (   A.long "magnitude"
            <> A.short 'm'
            <> A.metavar "MAGNITUDE"
            <> A.help "Magnitude of brightness change"))
    <*> outputOption
    <*> pure T.changeBrightness

contrast ∷ Parser CommandType
contrast = ArgT
    <$> inputOption
    <*> (A.option A.auto
           (   A.long "magnitude"
            <> A.short 'm'
            <> A.metavar "MAGNITUDE"
            <> A.help "Magnitude of contrast change"))
    <*> outputOption
    <*> pure T.changeContrast

red ∷ Parser CommandType
red =  ArgT
   <$> inputOption
   <*> magnitudeOption
   <*> outputOption
   <*> pure T.changeRed

green ∷ Parser CommandType
green =  ArgT
     <$> inputOption
     <*> magnitudeOption
     <*> outputOption
     <*> pure T.changeGreen

blue ∷ Parser CommandType
blue =  ArgT
    <$> inputOption
    <*> magnitudeOption
    <*> outputOption
    <*> pure T.changeBlue

flip ∷ Parser CommandType
flip = SingleT <$> inputOption <*> outputOption <*> pure T.flip

flipVertical ∷ Parser CommandType
flipVertical = SingleT <$> inputOption <*> outputOption <*> pure T.flipVertical

add ∷ Parser CommandType
add = MultiT
    <$> (A.many $ A.strOption
           (   A.long "img"
            <> A.metavar "IMAGE"
            <> A.help "Image to be added"))
    <*> outputOption
    <*> pure (foldl1 Arith.add)

and ∷ Parser CommandType
and =   MultiT
    <$> (A.many $ A.strOption
          (   A.long "img"
           <> A.metavar "IMAGE"
           <> A.help "Image to be and'ed"))
    <*> outputOption
    <*> pure (foldl1 Arith.and)

or ∷ Parser CommandType
or = MultiT
    <$> (A.many $ A.strOption
          (   A.long "img"
           <> A.metavar "IMAGE"
           <> A.help "Image to be or'ed"))
    <*> outputOption
    <*> pure (foldl1 Arith.or)

menu ∷ Parser CommandType
menu = A.subparser
         $  A.command "brightness"
             (A.info brightness
                      (A.progDesc "Change brightness of given image."))
         <> A.command "contrast"
             (A.info contrast
                      (A.progDesc "Change contrast of given image."))
         <> A.command "flip"
             (A.info flip
                      (A.progDesc "Flip a given image about the origin."))
         <> A.command "flipVertical"
             (A.info flipVertical
                     (A.progDesc "Flip a given image vertically."))
         <> A.command "add"
             (A.info add
                     (A.progDesc "Add one or more images together."))
         <> A.command "red"
             (A.info red
                     (A.progDesc "Change the red component of a given image."))
         <> A.command "blue"
             (A.info blue
                     (A.progDesc "Change the blue component of a given image."))
         <> A.command "green"
              (A.info green
                      (A.progDesc "Change the green component of a given image."))
         <> A.command "and"
              (A.info and
                      (A.progDesc "Bitwise-and multiple images together."))
         <> A.command "or"
              (A.info or
                      (A.progDesc "Bitwise-or multiple images together."))

unwrapImage ∷ DynamicImage → Maybe (Image PixelRGBA8)
unwrapImage (ImageRGBA8 img) = Just img
unwrapImage (ImageRGB8 img)  = Just (T.addAlphaChannel img)
unwrapImage _                = Nothing

run ∷ CommandType → IO ()
run (SingleT inFile outFile f) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left  error → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile $ f img
      _              → putStrLn "Type not handled yet."
run (MultiT imgPaths outFile f) = do
  imgsLoad ← mapM readImage imgPaths
  case partitionEithers imgsLoad of
    ([], images@(_:_)) →
      case mapM unwrapImage images of
        Just imgs → writePng outFile $ f imgs
        Nothing   → putStrLn "Type not handled yet."
    (errs, _)  → mapM_ putStrLn errs
run (ArgT inFile n outFile f) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left error  → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile $ f n img
      _              → putStrLn "Type not handled yet."

main ∷ IO ()
main = let opts = A.info (A.helper <*> menu)
                  (   A.fullDesc
                   <> A.progDesc "Process images."
                   <> A.header "pixs")
       in A.execParser opts >>= run
