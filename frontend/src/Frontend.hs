{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Reflex.Dom.SemanticUI
import qualified Reflex.Dom.SemanticUI as S


import Control.Lens

import Common.Api
import Static
import Data.Map (Map, fromList)
import Data.Monoid ((<>))

import           Language.Javascript.JSaddle.Types  (liftJSM, JSString, JSVal, JSM, MonadJSM)

import           Language.Javascript.JSaddle        (jss, js0, js1, js2,  jsg, create, setProp, textToJSString, toJSVal, js,
                                                     toJSString, getProp, obj, fun)


import Data.ByteString

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Bugadex Minimal Example"
      styleSheet "styling.css"
      styleSheet "semantic.min.css"
      styleSheet "extra.css"

      where
        styleSheet linkvar = elAttr "link" (fromList [
                                              ("rel","stylesheet")
                                            , ("type", "text/css")
                                            , ("href", linkvar)
                                            ]) $ return ()

    body= bod 400 400

-- | This loads the JS libs, and then returns an event that fires when they have all loaded.
loadJSLibs :: MonadWidget t m => m (Event t ())
loadJSLibs = do
  let
      custSrc = "customVis.js"
      awsSrc = "aws-sdk-2.282.1.min.js"
  (cust_script, _) <- elAttr' "script" ("src" =: custSrc) blank
  (aws_script, _) <- elAttr' "script" ("src" =: awsSrc) blank

  let
      addOne a = a + 1
      addOne :: Integer -> Integer
      awsLoaded = (addOne) <$ domEvent Load aws_script
      custScriptLoaded = (addOne) <$ domEvent Load cust_script
      eventList = [awsLoaded,  custScriptLoaded]
      lenEventList = Prelude.length eventList
  numberLoaded <- foldDyn ($) 0 $ mergeWith (.) eventList

  let ready = ffilter (==(fromIntegral lenEventList)) $ updated numberLoaded

  pure $ () <$ ready

semanticLogo :: MonadWidget t m => m ()
--semanticLogo = image (def & imageConfig_size |?~ Massive
--                    ) $ Left $ Img url def
--  where url = "/bugadex.png"
semanticLogo = do
  elAttr "img" ( "src" =: "/bugadex.png" <> "style" =: "width: 20rem") $ blank


bod :: MonadWidget t m => Int -> Int -> m ()
bod w h = do

  let mainConfig = def
                         & elConfigAttributes |~ ("id" =: "main")
                         & elConfigClasses |~ "ui container centered"
  ui "div" mainConfig $ segment def $ do

    let tshow = T.pack . show
    let dimensions ="width"=:(tshow w)<>"height"=:(tshow h)
        dimensions :: Map T.Text T.Text
    (click) <- divClass "ui grid centered" $ do
      semanticLogo
      divider def
      divClass "ui row" $ elAttr "video" ("id"=:"video"<>dimensions <> "autoplay"=:"" <> "playsinline"=:"" <> "controls"=:"true") $ blank
      c <- divClass "ui row " $ do
        c <- S.button def $ text "Snap Photo"

        pure (c)
      divClass "ui row" $ elAttr "canvas" (dimensions <> "id"=:"canvas") $ text "Please wait untill video is ready..."
      segment (def & segmentConfig_elConfig . elConfigAttributes |~ ("id" =: "output") ) $ text "no results yet"

      pure (c)

    jsReady <- loadJSLibs
    pb <- getPostBuild
    post <- delay 0.2 pb
    widgetHold blank $ (const (cameraStart)) <$> post
    up <- delay 1 click
    widgetHold blank $ (const (takePhoto (show w) (show h))) <$> click
    widgetHold blank $ (const uploadPhoto) <$> up
    -- widgetHold blank $ (const runResult) <$> res
    blank




makeVideoSettingsObject :: MonadJSM m => m (JSVal)
makeVideoSettingsObject = liftJSM $ do

  j <- obj
  j ^. jss ("ideal" :: String) ("environment" :: String)

  k <- obj
  k ^. jss ("facingMode" :: String) j

  o <- obj
  o ^. jss ("audio" :: String) (False)
  o ^. jss ("video" :: String) k

  toJSVal o



-- | Implementation of the cameraStart function written in pure JS.
cameraStart :: MonadJSM m =>  m ()
cameraStart = do
  conf <- makeVideoSettingsObject
  liftJSM $ do
    doc <- jsg ("document" :: String)
    con <- jsg ("console" :: String)

    video <- doc ^. js1 ("getElementById" :: String) ("video" :: String)
    nav <- jsg ("navigator" :: String)

    mediaDevices <- nav ^. js ("mediaDevices" :: String)

    res <- mediaDevices ^. js1 ("getUserMedia" :: String) conf


    res1 <- res ^. js1 ("then" :: String) ( fun $ \ _ _ [stream] -> do
                                      video ^. jss ("srcObject" :: String) stream
                                      video ^. js0 ("play" :: String)
                                      video ^. js1 ("removeAttribute" :: String) ("controls" :: String)
                                      return ()
                                  )

    res1 ^. js1 ("catch" :: String) ( fun $ \ _ _ [err] -> do
                                       con ^. js1 ("log" :: String) err

                                       return ()
                                  )


    pure ()


takePhoto :: (MonadWidget t m, MonadJSM m) =>  String -> String -> m ()
takePhoto w h = do
  liftJSM $ do
    photoTaker <- jsg ("photo_settings" :: String)

    photoTaker ^. js2 ("takepicture" :: String) w h
    uploader <- jsg ("uploader" :: String)
    uploader ^. js0 ("init" :: String)
  pure ()

uploadPhoto :: (MonadWidget t m, MonadJSM m) =>  m ()
uploadPhoto = do
  liftJSM $ do
    uploader <- jsg ("uploader" :: String)
    uploader ^. js0 ("upload" :: String)
  pure ()


runResult :: (MonadWidget t m, MonadJSM m) =>  m ()
runResult = do
  liftJSM $ do
    resulter <- jsg ("resulter" :: String)
    resulter ^. js0 ("getResult" :: String)
  pure ()
