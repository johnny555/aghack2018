{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Static

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

  (cust_script, _ ) <- elAttr' "script" ("src" =: custSrc) blank

  pure $ () <$ domEvent Load cust_script



bod :: MonadWidget t m => m ()
bod = do
  jsReady <- loadJSLibs
  text "Welcome to Obelisk!"
  el "p" $ text $ T.pack commonStuff
  elAttr "img" ("src" =: static @"obelisk.jpg") blank
  blank
