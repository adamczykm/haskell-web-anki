module Keyboard where

import Reflex.Dom
import Control.Monad.IO.Class
import Control.Monad (void)
import GHCJS.DOM.Types (Element, toElement)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM (currentDocument)

keyPressedQuery :: MonadWidget t m => Int -> m (Event t ())
keyPressedQuery keycode = do
  bdy <- currentBodyUnchecked
  return $ void (ffilter (keycode ==) (domEvent Keypress bdy))

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
