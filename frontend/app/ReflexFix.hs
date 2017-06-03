{-# LANGUAGE LambdaCase #-}
module ReflexFix where

import Reflex
import Data.Align
import Data.These

-- | Attach the current value of the 'Dynamic' to the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachPromptlyDyn d` is not the same as `attach (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachPromptlyDyn = attachPromptlyDynWith (,)

-- | Combine the current value of the 'Dynamic' with the value of the
-- 'Event' each time it occurs.
--
-- Note: `attachPromptlyDynWith f d` is not the same as `attachWith f (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWith f = attachPromptlyDynWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining the value at each occurence with the
-- current value of the 'Dynamic' value and possibly filtering if the combining
-- function returns 'Nothing'.
--
-- Note: `attachPromptlyDynWithMaybe f d` is not the same as `attachWithMaybe f
-- (current d)`.  See 'tagPromptlyDyn' for details.
attachPromptlyDynWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
attachPromptlyDynWithMaybe f d e =
  let e' = attach (current d) e
  in fforMaybe (align e' $ updated d) $ \case
       This (a, b) -> f a b -- Only the tagging event is firing, so use that
       These (_, b) a -> f a b -- Both events are firing, so use the newer value
       That _ -> Nothing -- The tagging event isn't firing, so don't fire

-- | Replace the value of the 'Event' with the current value of the 'Dynamic'
-- each time the 'Event' occurs.
--
-- Note: `tagPromptlyDyn d e` differs from `tag (current d) e` in the case that `e` is firing
-- at the same time that `d` is changing.  With `tagPromptlyDyn d e`, the *new* value of `d`
-- will replace the value of `e`, whereas with `tag (current d) e`, the *old* value
-- will be used, since the 'Behavior' won't be updated until the end of the frame.
-- Additionally, this means that the output 'Event' may not be used to directly change
-- the input 'Dynamic', because that would mean its value depends on itself.  When creating
-- cyclic data flows, generally `tag (current d) e` is preferred.
tagPromptlyDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagPromptlyDyn = attachPromptlyDynWith const
