module Debug where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Reflex.Dom (performEvent_)

debugPrint b = seq (unsafePerformIO (print b)) b
debugPrint2 str b = seq (unsafePerformIO (putStrLn str)) b
debugPrintF b = seq (fmap (unsafePerformIO . print)b) b

debugPrintEvent e = performEvent_ $ liftIO . print <$> e
