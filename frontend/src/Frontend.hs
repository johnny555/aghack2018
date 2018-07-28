{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Static

import Data.Monoid ((<>))

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Bugadex Minimal Example"
    body= bod

-- | This loads the JS libs, and then returns an event that fires when they have all loaded.
loadJSLibs :: MonadWidget t m => m (Event t ())
loadJSLibs = do
  let
      custSrc = "customVis.js"

  (cust_script, _) <- elAttr' "script" ("src" =: custSrc) blank

  pure $ () <$ domEvent Load cust_script



bod :: MonadWidget t m => m ()
bod = do
  jsReady <- loadJSLibs
  text "Welcome to BugADex!"
  el "p" $ text $ "Take a photo of a bug!"
  let dimensions ="width"=:"640"<>"height"=:"480"
  elAttr "video" ("id"=:"video"<>dimensions <> "autoplay"=:"") blank
  elAttr "button" ("id"=:"snap") $ text "Snap Photo"
  elAttr "canvas" (dimensions <> "id"=:"canvas") blank
  blank
