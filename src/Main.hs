import Control.Applicative
import Prelude hiding (flip)
import System.Environment (getArgs)
import Codec.Picture (readImage, writePng, DynamicImage(..))
import Transformations


main = do
  -- TODO: There is probably a much better way of
  -- doing argument parsing here.
  args           <- getArgs
  imageLoad      <- readImage (args !! 1)
  case imageLoad of
    Left  error -> putStrLn error
    Right image ->
      case image of
          ImageRGBA8 img ->
            case args of
              ("brightness":_:amount:outFile:[])
                -> writePng outFile
                   $ changeBrightness (read amount) img
              ("red":_:amount:outFile:[])
                -> writePng outFile
                   $ changeRed (read amount) img
              ("green":_:amount:outFile:[])
                -> writePng outFile
                   $ changeGreen (read amount) img
              ("flipVertical":_:outFile:[])
                 -> writePng outFile
                    $ flipVertical img
              ("flipHorizontal":_:outFile:[])
                 -> writePng outFile
                    $ flipHorizontal img
              ("flip":_:outFile:[])
                -> writePng outFile
                   $ flip img
              ("blur":_:outFile:[])
                -> writePng outFile
                   $ blur img
              _ -> putStrLn "Please enter valid arguments."
          ImageRGB8 img ->
            case args of
              ("flip":_:outFile:[])
                -> writePng outFile
                   $ flip img
              ("flipHorizontal":_:outFile:[])
                -> writePng outFile
                   $ flipHorizontal img
              ("flipVertical":_:outFile:[])
                -> writePng outFile
                   $ flipVertical img
              _ -> putStrLn "Please enter valid arguments."
          ImageY8 img -> putStrLn "Type not handled yet."
          ImageY16 img -> putStrLn "Type not handled yet."
          ImageYF img -> putStrLn "Type not handled yet."
          ImageYA8 img -> putStrLn "Type not handled yet."
