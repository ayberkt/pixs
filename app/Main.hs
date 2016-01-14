{-# LANGUAGE UnicodeSyntax #-}

import           Codec.Picture              (DynamicImage (..), readImage,
                                             writePng)
-- import qualified Pixs.Filter                as F
-- import qualified Pixs.Information.Histogram as H
import qualified Pixs.Transformation        as T
import qualified Pixs.Arithmetic            as Arith
import           Prelude                    hiding (error, flip)
import qualified Options.Applicative        as A
import           Options.Applicative        (Parser, (<>))

data Command = Brightness FilePath Int      FilePath
             | Flip       FilePath FilePath
             | Add        FilePath FilePath FilePath
             deriving (Eq, Read)

inputOption ∷ Parser FilePath
inputOption = A.strOption (   A.long "in"
                           <> A.metavar "INPUT"
                           <> A.help "Input image file to be transformed")

outputOption ∷ Parser FilePath
outputOption = A.strOption (   A.long "out"
                            <> A.metavar "OUTPUT"
                            <> A.help "File name to write to.")

brightness ∷ Parser Command
brightness = Brightness
    <$> inputOption
    <*> (A.option A.auto
           (   A.long "magnitude"
            <> A.short 'm'
            <> A.metavar "MAGNITUDE"
            <> A.help "Magnitude of brightness change"))
    <*> outputOption

flip ∷ Parser Command
flip = Flip <$> inputOption <*> outputOption

add ∷ Parser Command
add = Add
    <$> A.strOption (   A.long "img1"
                     <> A.metavar "OPERAND₁"
                     <> A.help "First operand image")
    <*> A.strOption (   A.long "img2"
                     <> A.metavar "OPERAND₂"
                     <> A.help "Second operand image")
    <*> outputOption

menu ∷ Parser Command
menu = A.subparser
         $  A.command "brightness"
             (A.info brightness
                      (A.progDesc "Change brightness of given image."))
         <> A.command "flip"
             (A.info flip
                      (A.progDesc "Flip a given image about the origin."))
         <> A.command "add"
             (A.info add
                     (A.progDesc "Add two images together."))

run ∷ Command → IO ()
run (Brightness inFile n outFile) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left error  → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile
                         $ T.changeBrightness n img
      _              → putStrLn "Type not handled yet."
run (Flip inFile outFile) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left  error → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile
                         $ T.flip img
      _              → putStrLn "Type not handled yet."
run (Add imgPath₁ imgPath₂ outFile) = do
  imgLoad₁ ← readImage imgPath₁
  imgLoad₂ ← readImage imgPath₂
  case (imgLoad₁, imgLoad₂) of
    (Right (ImageRGBA8 img₁), Right (ImageRGBA8 img₂)) →
      writePng outFile
      $ Arith.add img₁ img₂
    _ → putStrLn "An error has occured."

main ∷ IO ()
main = let opts = A.info (A.helper <*> menu)
                  (   A.fullDesc
                   <> A.progDesc "Process images."
                   <> A.header "pixs")
       in A.execParser opts >>= run
