--
--  Imgur Uploader
--  Dan Colish <dcolish@gmail.com>
--  Copyright (c) 2010, 2011
--  All rights reserved
--
--

module Network.Imgurder (
  ImgurUpload(ImgurUpload, ImgurFailure),
  upload
  ) where

import Data.IORef
import Data.Maybe
import Data.Tree.Class
import Network.Curl
import Network.URL
import Text.XML.HXT.DOM.TypeDefs
import qualified Text.XML.HXT.DOM.XmlNode as N
import qualified Text.XML.HXT.Parser.XmlParsec as H
import qualified Text.XML.HXT.XPath.XPathEval as X


data ImgurUpload = ImgurFailure Int
    | ImgurUpload {
        imageHash :: String,
        deleteHash :: String,
        originalImage :: URL,
        largeThumbnail :: URL,
        smallThumbnail :: URL,
        imgurPage :: URL,
        deletePage :: URL
     }


instance Show ImgurUpload where
    show (ImgurUpload _ _ oi lt st ip dp) = unlines ["Image link: " ++ exportURL oi,
        "Large thumbnail: " ++ exportURL lt,
        "Small thumbnail: " ++ exportURL st,
        "Imgur page link: " ++ exportURL ip,
        "Delete page link: " ++ exportURL dp]
    show (ImgurFailure a) = "Something has gone wrong, try again later!\n Response: " ++ show a


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


tagPaths :: [String]
tagPaths = [ "/rsp/image_hash"
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
keyVal res str = (xpathQN str res, xpathTxt str res)


formattedResult :: XmlTree -> [(String, String)]
formattedResult res = map (keyVal res) tagPaths


imgurify :: [(String, String)] -> Maybe ImgurUpload
imgurify xs = do
    imageHash' <- lookup "image_hash" xs
    deleteHash' <- lookup "delete_hash" xs
    originalImage' <- lookup "original_image" xs >>= importURL
    largeThumbnail' <- lookup "large_thumbnail" xs >>= importURL
    smallThumbnail' <- lookup "small_thumbnail" xs >>= importURL
    imgurPage' <- lookup "imgur_page" xs >>= importURL
    deletePage' <- lookup "delete_page" xs >>= importURL
    return $ ImgurUpload imageHash' deleteHash' originalImage' largeThumbnail' smallThumbnail' imgurPage' deletePage'


curlMultiPost' :: URLString -> [CurlOption] -> [HttpPost] -> IO Int
curlMultiPost' s os ps = do
  h <- initialize
  setopt h (CurlVerbose True)
  setopt h (CurlURL s)
  setopt h (CurlHttpPost ps)
  mapM_ (setopt h) os
  perform h
  getResponseCode h


upload :: String -> FilePath -> IO (Either ImgurUpload ImgurUpload)
upload key file = withCurlDo $ do
    ref <- newIORef []
    resp <- curlMultiPost' "http://api.imgur.com/1/upload.xml"
            [CurlWriteFunction (gatherOutput ref), CurlVerbose False]
            $ myCurlPost key file
    case resp of
      200 -> do
        response <- fmap reverse $ readIORef ref
        let imgurUpload = imgurify . formattedResult . result $ response
        case imgurUpload of
          Just a -> return . Right $ a
          Nothing -> return . Left $ ImgurFailure (-1)
      _ -> return . Left $ ImgurFailure resp
    where
        result = head . H.xread . concatMap (unwords.tail.lines)
