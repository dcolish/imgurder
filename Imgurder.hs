--
--  Imgur Uploader
--  Dan Colish <dcolish@gmail.com>
--  Copyright (c) 2010, 2011
--  All rights reserved
--
--  Usage: imgurder <filepath>
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
key = "a05aaffbb595c38c2cd2d54d4c57eb3f"


data ImgurUpload = ImgurUpload {
    imageHash :: String,
    deleteHash :: String,
    originalImage :: String,
    largeThumbnail :: String,
    smallThumbnail :: String,
    imgurPage :: String,
    deletePage :: String
    }

instance Show ImgurUpload where
    show (ImgurUpload _ _ oi lt st ip dp) = unlines ["Image link: " ++ oi,
        "Large thumbnail: " ++ lt,
        "Small thumbnail: " ++ st,
        "Imgur page link: " ++ ip,
        "Delete page link: " ++ dp]


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
    h <- initialize
    ref <- newIORef []
    curlMultiPost "http://imgur.com/api/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False, CurlFailOnError True]
            $ myCurlPost key file
    resp <- getResponseCode h
    case resp of
      200 -> do
        response <- fmap reverse $ readIORef ref
        return . imgurify . formattedResult . result $ response
      _ -> return Nothing
    where
        result = head . H.xread . concatMap (unwords.tail.lines)
