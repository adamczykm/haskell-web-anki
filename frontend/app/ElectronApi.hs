{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module ElectronApi where

import qualified Data.JSString as S


---------------- API --------------------

class Monad m => MonadElectronApi m where
    consoleLog :: String -> m ()
    alert :: String -> m ()
    showOpenDialog :: m ()

---------- Implementations -------------

instance MonadElectronApi IO where
  consoleLog = js_console_log . S.pack
  alert = js_alert . S.pack
  showOpenDialog = js_show_open_dialog

---------- JS imports --------------

foreign import javascript unsafe
  "require('console').log($1)" js_console_log :: S.JSString -> IO ()

foreign import javascript unsafe
  "alert($1)" js_alert :: S.JSString -> IO ()

-- foreign import javascript unsafe
--     "alert('kopa')" js_show_open_dialog :: IO ()
-- foreign import javascript unsafe
--     "require('electron').showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']});" js_show_open_dialog :: IO ()

-- foreign import javascript interruptible
--     "require('fs').access('/etc/passwd', fs.constants.R_OK | fs.constants.W_OK, (err) => {console.log(err ? 'no access!' : 'can read/write');});" js_show_open_dialog :: IO ()
foreign import javascript unsafe
    "require('console').log(require('fs').readFileSync('/etc/passwd'));" js_show_open_dialog :: IO ()

-- foreign import javascript interruptible "const {dialog} = require('electron'); console.log(dialog.showOpenDialog({properties: ['openFile', 'openDirectory', 'multiSelections']},$c))"
--   js_show_open_dialog :: IO ([S.JSString])
