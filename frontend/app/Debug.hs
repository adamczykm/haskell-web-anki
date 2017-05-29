module Debug where

import System.IO.Unsafe (unsafePerformIO)

debugPrint b = seq (unsafePerformIO (print b)) b
debugPrint2 str b = seq (unsafePerformIO (putStrLn str)) b
debugPrintF b = seq (fmap (unsafePerformIO . print)b) b