{-# LANGUAGE UnicodeSyntax #-}

import           Codec.Picture              (DynamicImage (..), readImage,
                                             writePng)
import qualified Pixs.Filter                as F
import qualified Pixs.Information.Histogram as H
import qualified Pixs.Transformation        as T
import           Prelude                    hiding (error, flip)
import           System.Environment         (getArgs)
import qualified Options.Applicative        as A
import           Options.Applicative        (Parser, (<>))

data Command = Brightness String Int    String
             | Flip       String String
             deriving (Eq, Read)

brightness ∷ Parser Command
brightness = let toInt x = (read x) ∷ Int
             in Brightness
                <$> A.strOption (   A.long "in"
                                 <> A.metavar "INPUT"
                                 <> A.help "Input image file to be transformed")
                <*> fmap toInt
                      (A.strOption
                        (   A.long "magnitude"
                         <> A.short 'm'
                         <> A.metavar "MAGNITUDE"
                         <> A.help "Magnitude of brightness change"))
                <*> A.strOption
                      (   A.long "out"
                       <> A.metavar "OUTPUT"
                       <> A.help "File name to write to.")

flip ∷ Parser Command
flip = Flip
    <$> A.strOption (  A.long "in"
                    <> A.metavar "INPUT"
                    <> A.help "Input image file to be transformed.")
    <*> A.strOption
    (   A.long "out"
     <> A.metavar "OUTPUT"
     <> A.help "File name to write to.")

menu ∷ Parser Command
menu = A.subparser
         $  A.command "brightness"
             (A.info brightness
                      (A.progDesc "Change brightness of given image."))
         <> A.command "flip"
             (A.info flip
                      (A.progDesc "Flip a given image about the origin."))

run ∷ Command → IO ()
run (Brightness inFile n outFile) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left error  → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile
                         $ T.changeBrightness n img
run (Flip inFile outFile) = do
  imageLoad ← readImage inFile
  case imageLoad of
    Left  error → putStrLn error
    Right image → case image of
      ImageRGBA8 img → writePng outFile
                         $ T.flip img

main ∷ IO ()
main = let opts = A.info (A.helper <*> menu)
                  (   A.fullDesc
                   <> A.progDesc "Process images."
                   <> A.header "pixs")
       in A.execParser opts >>= run

-- main ∷ IO ()
-- main = do
--   -- TODO: There is probably a much better way of
--   -- doing argument parsing here.
--   args           <- getArgs
--   imageLoad      <- readImage (args !! 1)
--   case imageLoad of
--     Left  error → putStrLn error
--     Right image →
--       case image of
--           ImageRGBA8 img →
--             case args of
--               ["brightness",_,amount,outFile]
--                 → writePng outFile
--                     $ T.changeBrightness (read amount) img
--               ["red",_,amount,outFile]
--                 → writePng outFile
--                     $ T.changeRed (read amount) img
--               ["green",_,amount,outFile]
--                 → writePng outFile
--                     $ T.changeGreen (read amount) img
--               ["flipVertical",_,outFile]
--                 → writePng outFile
--                     $ T.flipVertical img
--               ["flipHorizontal",_,outFile]
--                 → writePng outFile
--                     $ T.flipHorizontal img
--               ["flip",_,outFile]
--                 → writePng outFile
--                    $ T.flip img
--               ["blur",_,amount,outFile]
--                 → writePng outFile
--                     $ T.blur img (read amount)
--               ["saturation",_,amount,outFile]
--                 → writePng outFile
--                     $ T.saturation (read amount) img
--               ["negate",_,outFile]
--                 → writePng outFile
--                     $ T.negateImage img
--               ["--black-and-white",_,outFile]
--                 → writePng outFile
--                     $ F.blackAndWhite img
--               ["--pixelate",_,outFile]
--                 → writePng outFile
--                     $ F.pixelate img
--               ["redCount",_]
--                 → putStrLn . show $ H.redCount img
--               ["histogram",_]
--                 → H.makeHistogram img
--               _ → putStrLn "Please enter valid arguments."
--           ImageRGB8 img →
--             case args of
--               ["flip",_,outFile]
--                 → writePng outFile
--                    $ T.flip img
--               ["flipHorizontal",_,outFile]
--                 → writePng outFile
--                    $ T.flipHorizontal img
--               ["flipVertical",_,outFile]
--                 → writePng outFile
--                    $ T.flipVertical img
--               _ → putStrLn "Please enter valid arguments."
--           ImageY8     _ → putStrLn "Type not handled yet."
--           ImageY16    _ → putStrLn "Type not handled yet."
--           ImageYF     _ → putStrLn "Type not handled yet."
--           ImageYA8    _ → putStrLn "Type not handled yet."
--           ImageYA16   _ → putStrLn "Type not handled yet."
--           ImageRGBA16 _ → putStrLn "Type not handled yet."
--           ImageRGBF   _ → putStrLn "Type not handled yet."
--           ImageRGB16  _ → putStrLn "Type not handled yet."
--           ImageYCbCr8 _ → putStrLn "Type not handled yet."
--           ImageCMYK8  _ → putStrLn "Type not handled yet."
--           ImageCMYK16 _ → putStrLn "Type not handled yet."
