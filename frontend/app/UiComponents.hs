
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module UiComponents where

import           Data.Map   (fromList)
import           Reflex.Dom
import Domain
import ApiClient (imgSrcForQuestion)
import Data.Text (unpack)


elDiv' = elClass' "div"

-- fullSizeDivWithClass' :: forall t m a. DomBuilder t m => Text -> Map Text Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
fullSizeDivWithClass' :: MonadWidget t m => String -> m a -> m (El t ,a)
fullSizeDivWithClass' cls = elAttr' "div" (fromList [("class",cls),("height","100%"),("max-height","100%"),("max-width","100%")])
-- fullSizeDivWithClass' :: MonadWidget t m => String -> m a -> m (El t ,a)
-- fullSizeDivWithClass' cls = elAttr' "div" (fromList [("class",cls),("max-height","100%"),("max-width","100%")])

fullSizeDivWithClass :: MonadWidget t m => String -> m a -> m a
fullSizeDivWithClass cls w = snd <$> fullSizeDivWithClass' cls w


centeringDiv' :: MonadWidget t m => m a -> m (El t ,a)
centeringDiv' w = elAttr' "div" (fromList [("position","absolute"),("height","auto"),("width","auto"),("text-align","center")]) $ do
  _ <- elAttr' "span" (fromList [("display","inline-block"),("height","100%"),("vertical-align","middle")]) $ return ()
  w

centeringDiv :: MonadWidget t m => m a -> m a
centeringDiv w = snd <$> centeringDiv' w


fullSizeCenteredImg' :: MonadWidget t m => String -> m (El t, String)
fullSizeCenteredImg' src = centeringDiv' $
  elAttr "img" (fromList [("src",src),("max-height","100%"),("max-width","100%")]) $
  return src

fullSizeCenteredImg :: MonadWidget t m => String -> m String
fullSizeCenteredImg src = snd <$> fullSizeCenteredImg' src

divQuestionPrompt' :: MonadWidget t m => m a -> m (El t, a)
divQuestionPrompt' = elAttr' "div" (fromList [("position","relative"),("height","100%"),("overflow","hidden")]) . elAttr "div" (fromList [("position","absolute"),("top","0"),("left","0"),("right","0"),("bottom","0"),("color","white")]) . elClass "div" " "

divQuestionPrompt :: MonadWidget t m => m a -> m a
divQuestionPrompt w = snd <$> divQuestionPrompt' w


divQuestionAnswer' :: MonadWidget t m => m a -> m (El t, a)
divQuestionAnswer' = elAttr' "div" (fromList [("position","relative"),("height","80%"),("overflow","hidden")]) . elAttr "div" (fromList [("position","absolute"),("top","0"),("left","0"),("right","0"),("bottom","0"),("color","white")]) . elClass "div" " ". elClass "span" ""

divQuestionAnswer :: MonadWidget t m => m a -> m a
divQuestionAnswer w = snd <$> divQuestionPrompt' w

-- spanQuestion :: MonadWidget t m => Question -> m (El t, ())
-- spanQuestion (Question _ _ q) = elAttr' "span" (fromList [("padding","20px 20px 20px 20px"),("font-size","300%")]) $ text $ unpack q

spanQuestion :: MonadWidget t m => Question -> m (El t, ())
spanQuestion (Question _ _ q) = elAttr' "div" (fromList [("style","\"padding\":\"20px 20px 20px 20px\";\"font-size\":\"300%\"")]) $ text $ unpack q

questionImg :: MonadWidget t m => Question -> m (El t, Question)
questionImg q = elAttr' "img" (fromList [("src",imgSrcForQuestion q),("display","block"),("margin","auto"),("width","100%")]) $ return q


divAnswerButtons :: MonadWidget t m => m a -> m a
divAnswerButtons = elAttr "div" (fromList [("position","relative"),("height","20%"),("overflow","hidden")])
