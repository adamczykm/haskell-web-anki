module WorkflowUtils where

import Prelude hiding (lookup)
import Reflex.Dom
import Data.Map (Map, lookup)

newtype WorkflowWidget t m k a = MkWorkflowWidget { _unWorkflowWidget :: a -> m (a, Event t (a, Maybe k))}

widgetGraphWorkflow :: (Ord k, MonadWidget t m) => Map k (WorkflowWidget t m k a) -> a -> k -> Workflow t m a
widgetGraphWorkflow g' start' k' = widgetGraphWorkflowRec g' start' (Just k')
  where
    widgetGraphWorkflowRec :: (Ord k, MonadWidget t m) => Map k (WorkflowWidget t m k a) -> a -> Maybe k -> Workflow t m a
    widgetGraphWorkflowRec _ start Nothing  = Workflow $ return (start, never)
    widgetGraphWorkflowRec g start (Just k) = Workflow $ case lookup k g of
        Nothing -> return (start, never)
        Just ww -> do
          (v, wfEvent) <- _unWorkflowWidget ww start
          return (v, fmap (uncurry (widgetGraphWorkflowRec g)) wfEvent)
