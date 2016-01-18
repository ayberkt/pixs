{-# LANGUAGE UnicodeSyntax, GADTs, ExistentialQuantification #-}

import           Data.Either                (partitionEithers)
import           Codec.Picture              (DynamicImage (..), Image,
                                             PixelRGBA8, readImage, writePng)
-- import qualified Pixs.Filter                as F
-- import qualified Pixs.Information.Histogram as H
import qualified Pixs.Transformation        as T
import qualified Pixs.Arithmetic            as Arith
import qualified Pixs.PointOperations       as PO
import           Prelude                    hiding (error, and, or)
import qualified Options.Applicative        as A
import           Options.Applicative        (Parser, (<>))

data CommandType where
  SingleT ∷ FilePath
          → FilePath
          → (Image PixelRGBA8 → Image PixelRGBA8)
          → CommandType
  MultiT ∷ [FilePath]
         → FilePath
         → ([Image PixelRGBA8] → Image PixelRGBA8)
         → CommandType
  ArgT ∷ forall a. Read a
       ⇒ FilePath
       → a
       → FilePath
       → (a → Image PixelRGBA8 → Image PixelRGBA8)
       → CommandType

data ReflectDirection = Origin | Horizontal | Vertical
  deriving (Show, Eq)

-- | Aliases for ReflectDirection.
instance Read ReflectDirection where
  readsPrec _ s | s `elem` ["h", "horizontal"] = [(Horizontal, [])]
                | s `elem` ["v", "vertical"]   = [(Vertical, [])]
                | s `elem` ["o", "origin"]     = [(Origin, [])]
                | otherwise                    = []

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
    <*> magnitudeOption
    <*> outputOption
    <*> pure T.changeBrightness

threshold ∷ Parser CommandType
threshold = ArgT
         <$> inputOption
         <*> magnitudeOption
         <*> outputOption
         <*> pure PO.threshold

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

reflect ∷ Parser CommandType
reflect = ArgT
    <$> inputOption
    <*> (A.option A.auto
           (   A.long "direction"
            <> A.short 'd'
            <> A.metavar "DIRECTION"
            <> A.help "Direction of reflect"))
    <*> outputOption
    <*> pure reflectDir
  where
    reflectDir Horizontal = T.reflectHorizontal
    reflectDir Vertical   = T.reflectVertical
    reflectDir Origin     = T.reflect

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

-- TODO: IMPLEMENT!
histogram ∷ Parser CommandType
histogram = undefined

menu ∷ Parser CommandType
menu = A.subparser
         $  A.command "brightness"
             (A.info brightness
               (A.progDesc "Change brightness of given image."))
         <> A.command "contrast"
             (A.info contrast
               (A.progDesc "Change contrast of given image."))
         <> A.command "reflect"
             (A.info reflect
                      (A.progDesc "Reflect a given image."))
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
         <> A.command "threshold"
              (A.info threshold
                (A.progDesc "Threshold given image."))

unwrapImage ∷ DynamicImage → Maybe (Image PixelRGBA8)
unwrapImage (ImageRGBA8 img) = Just img
unwrapImage (ImageRGB8 img)  = Just $ T.addAlphaChannel img
unwrapImage _                = Nothing

run ∷ CommandType → IO ()
run (SingleT inFile outFile f) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left  error → putStrLn error
    Right image → case image of
      ImageRGB8 img → writePng outFile . f $ T.addAlphaChannel img
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
      ImageRGB8 img → writePng outFile $ f n $ T.addAlphaChannel img
      ImageRGBA8 img → writePng outFile $ f n img
      _              → putStrLn "Type not handled yet."

main ∷ IO ()
main = let opts = A.info (A.helper <*> menu)
                  (   A.fullDesc
                   <> A.progDesc "Process images."
                   <> A.header "pixs")
       in A.execParser opts >>= run
