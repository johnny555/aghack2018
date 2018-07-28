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
import Data.Map (fromList)
import Data.Monoid ((<>))

import           Language.Javascript.JSaddle.Types  (liftJSM, JSString, JSVal, JSM, MonadJSM)

import           Language.Javascript.JSaddle        (jss, js0, js1, js2,  jsg, create, setProp, textToJSString, toJSVal)


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

    body= bod

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


bod :: MonadWidget t m => m ()
bod = do
  container def $ do
    pageHeader H1 def $ text "Welcome to BugADex!"
    subHeader  $ text $ "Take a photo of a bug!"
    let dimensions ="width"=:"640"<>"height"=:"480"
    (click, res) <- divClass "ui grid" $ do
      divClass "ui row" $ elAttr "video" ("id"=:"video"<>dimensions <> "autoplay"=:"") $ blank
      c <- divClass "ui row " $ do
        c <- S.button def $ text "Snap Photo"
        r <- S.button def $ text "Result "
        pure (c, r)
      divClass "ui row" $ elAttr "canvas" (dimensions <> "id"=:"canvas") $ text "Please wait untill video is ready..."
      segment (def & segmentConfig_elConfig . elConfigAttributes |~ ("id" =: "output") ) $ text "no results yet"
      pure c

    jsReady <- loadJSLibs

    up <- delay 0.5 click
    widgetHold blank $ (const takePhoto) <$> click
    widgetHold blank $ (const uploadPhoto) <$> up
    widgetHold blank $ (const runResult) <$> res
    blank



takePhoto :: (MonadWidget t m, MonadJSM m) =>  m ()
takePhoto = do
  liftJSM $ do
    photoTaker <- jsg ("photo_settings" :: String)

    photoTaker ^. js2 ("takepicture" :: String) ("640" :: String ) ("480" :: String)
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
