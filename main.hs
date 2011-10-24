import Imgurder
import System (getArgs)
import System.IO
import System.Exit

main :: IO ()
main = do
    [file] <- getArgs
    maybeImgurUpload <- upload file
    case maybeImgurUpload of
        Nothing -> hPutStrLn stderr "Something has gone wrong, try again later!" >> exitFailure
        Just imgurUpload -> print imgurUpload
