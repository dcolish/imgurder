--
--  Imgur Uploader
--  Dan Colish <dcolish@gmail.com>
--  Copyright (c) 2010
--  All rights reserved
--
--  Usage: runghc imgur.hs <filepath>
--  Be sure to add an api key before using it and put it in ~/imgurder
--  A sample configuration file would be:
--  `echo '<your api key>' > ~/.imgurder`
--
module Imgurder (ImgurUpload(ImgurUpload), upload) where

import Data.IORef
import Data.Maybe
import Data.Tree.Class
import Network.Curl
import Text.XML.HXT.DOM.TypeDefs
import qualified Text.XML.HXT.DOM.XmlNode as N
import qualified Text.XML.HXT.Parser.XmlParsec as H
import qualified Text.XML.HXT.XPath.XPathEval as X


key :: String
key = "27b25626f64a6a77fea07ec3ad2d5250"


data ImgurUpload = ImgurUpload {
    imageHash :: String,
    deleteHash :: String,
    originalImage :: String,
    largeThumbnail :: String,
    smallThumbnail :: String,
    imgurPage :: String,
    deletePage :: String
    } deriving (Show)


myCurlPost :: String -> String -> [HttpPost]
myCurlPost apikey myImage =
    [ HttpPost { postName = "image"
               , contentType = Nothing , content = ContentFile myImage
               , extraHeaders = []
               , showName = Nothing },
      HttpPost { postName = "key"
               , contentType = Nothing
               , content = ContentString apikey
               , extraHeaders = []
               , showName = Nothing }]


pathTags :: [String]
pathTags = [ "/rsp/image_hash"
           ,"/rsp/delete_hash"
           ,"/rsp/original_image"
           ,"/rsp/large_thumbnail"
           ,"/rsp/small_thumbnail"
           ,"/rsp/imgur_page"
           ,"/rsp/delete_page" ]


xpathQN:: String -> XmlTree -> String
xpathQN str = fromJust . N.getQualifiedName . getNode . head . X.getXPath str


xpathTxt:: String -> XmlTree -> String
xpathTxt str = fromJust . N.getText . head . getChildren . head . X.getXPath str


keyVal :: XmlTree -> String -> (String, String)
keyVal res str = (xpathQN str res,xpathTxt str res)


formattedResult :: XmlTree -> [(String, String)]
formattedResult res = map (keyVal res) pathTags


imgurify :: [(String, String)] -> Maybe ImgurUpload
imgurify xs = do
    imageHash' <- lookup "image_hash" xs
    deleteHash' <- lookup "delete_hash" xs
    originalImage' <- lookup "original_image" xs
    largeThumbnail' <- lookup "large_thumbnail" xs
    smallThumbnail' <- lookup "small_thumbnail" xs
    imgurPage' <- lookup "imgur_page" xs
    deletePage' <- lookup "delete_page" xs
    return $ ImgurUpload imageHash' deleteHash' originalImage' largeThumbnail' smallThumbnail' imgurPage' deletePage'


upload :: FilePath -> IO (Maybe ImgurUpload)
upload file = withCurlDo $ do
    _ <- initialize
    ref <- newIORef []
    curlMultiPost "http://imgur.com/api/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False]
            $ myCurlPost key file
    response <- fmap reverse $ readIORef ref
    return . imgurify . formattedResult .head . H.xread . concatMap (unwords.tail.lines) $ response
