{- | This module is a placeholder for commands.
In the future, the app will support instant command
execution to better interact with the MPD server.
Then it may include sorting/searching albums and
playlists, subscribing to remote MPD servers, etc.
-}
module Cmd (execCmd) where

import Brick.Types
import Types

execCmd :: String -> EventM (MName St) St ()
execCmd _cmd = pure () -- TODO: implement commands