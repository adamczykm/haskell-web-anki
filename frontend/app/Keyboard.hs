module Keyboard where

import Reflex.Dom
import Control.Monad.IO.Class
-- import Control.Monad (void)
import GHCJS.DOM.Types (Element, toElement)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM (currentDocument)

anyKeysDownQuery :: MonadWidget t m => [Int] -> m (Event t Int)
anyKeysDownQuery keys = do
  bdy <- currentBodyUnchecked
  return $ ffilter (`elem` keys) (domEvent Keydown bdy)

keyDownQuery :: MonadWidget t m => Int -> m (Event t Int)
keyDownQuery keycode = do
  bdy <- currentBodyUnchecked
  return $ ffilter (keycode ==) (domEvent Keydown bdy)

currentBodyUnchecked :: MonadWidget t m => m (El t)
currentBodyUnchecked = rawBodyUnchecked >>= wrapElement defaultDomEventHandler
  where
    rawBodyUnchecked :: MonadIO m => m Element
    rawBodyUnchecked = do
      mdoc <- liftIO currentDocument
      case mdoc of
        Nothing -> error "Document is nothing"
        Just doc -> do
          body <- getBody doc
          case body of
            Nothing -> error "body is nothing"
            Just htmlel -> return $ toElement htmlel
