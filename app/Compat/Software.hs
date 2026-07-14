{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | This module provides various support functions for
compatibility with different software such like PulseAudio.
-}
module Compat.Software (
  AudioServer (..),
  updateModuleEQId,
  restartMPDServer,
  restartAudioServer,
  spectrumUpdatingThread,
  extractExtraInfo,
  getMPDProcessId,
  getMPDSocket,
) where

import Brick qualified as B
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Exception
import Control.Monad (forever, replicateM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Binary.Get (getFloatle, runGet)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (uncons)
import Data.Maybe (catMaybes)
import Data.Scientific qualified as Sci
import Data.Text qualified as Txt
import Data.Vector qualified as Vec
import GHC.IO.Exception (ExitCode (..))
import Language.Haskell.TH.Syntax
import Lens.Micro.Mtl
import Network.MPD qualified as MPD
import System.Directory
import System.FilePath (isAbsolute, isRelative, makeRelative, normalise, splitDirectories, (</>))
import System.IO (hSetBinaryMode)
import System.Process (CreateProcess (..), StdStream (..))
import System.Process qualified as Sys
import Text.Printf (printf)
import Text.Read (readEither, readMaybe)
import Types
import Utils

-- | A data type to represent different audio servers.
data AudioServer
  = PipeWire -- For now, we only support PulseAudio
  deriving (Eq)

data SocketType = IPv4 | IPv6
  deriving (Eq, Show)

instance Read SocketType where
  readsPrec _ "IPv4" = [(IPv4, "")]
  readsPrec _ "IPv6" = [(IPv6, "")]
  readsPrec _ _ = []

instance Show AudioServer where
  show PipeWire = "pipewire"

{- | Capture the post-EQ PipeWire sink and publish logarithmic dBFS bands.
The request worker owns the enable flag so this thread never consumes MPD
requests directly.
-}
spectrumUpdatingThread :: BChan Event -> TVar Bool -> IO ()
spectrumUpdatingThread evChan enabled = forever $ do
  active <- atomically $ readTVar enabled
  if not active
    then threadDelay 100000
    else do
      void . tryJust @IOException (const $ Just ()) $
        Sys.withCreateProcess captureProcess $ \_ output _ _ ->
          case output of
            Nothing -> pure ()
            Just handle' -> do
              hSetBinaryMode handle' True
              let go = do
                    running <- atomically $ readTVar enabled
                    when running $ do
                      bytes <- BS.hGet handle' (2048 * 2 * 4)
                      when (BS.length bytes == 2048 * 2 * 4) $ do
                        writeBChan evChan $ UpdateSpectrum $ spectrumLevels bytes
                        go
              go
      threadDelay 1000000
 where
  captureProcess =
    ( Sys.proc
        "pw-cat"
        [ "--record"
        , "--raw"
        , "--format"
        , "f32"
        , "--rate"
        , "48000"
        , "--channels"
        , "2"
        , "--channel-map"
        , "FL,FR"
        , "--latency"
        , "20ms"
        , "--target"
        , "meloid_eq"
        , "--properties"
        , "{\"node.name\":\"meloid_spectrum\",\"media.category\":\"Monitor\",\"media.role\":\"DSP\",\"stream.capture.sink\":\"true\",\"stream.monitor\":\"true\"}"
        , "-"
        ]
    )
      { std_in = NoStream
      , std_out = CreatePipe
      , std_err = NoStream
      }

  spectrumLevels bytes =
    Vec.generate 64 $ \band ->
      let frequency = 30 * (18000 / 30) ** ((fromIntegral band + 0.5) / 64)
          (leftReal, leftImag, rightReal, rightImag) =
            foldl'
              ( \(lr, li, rr, ri) (index, (left, right)) ->
                  let weight = 0.5 * (1 - cos (2 * pi * fromIntegral index / 2047))
                      phase = 2 * pi * frequency * fromIntegral index / 48000
                      l = realToFrac left * weight
                      r = realToFrac right * weight
                   in (lr + l * cos phase, li + l * sin phase, rr + r * cos phase, ri + r * sin phase)
              )
              (0, 0, 0, 0)
              (zip [0 :: Int ..] frames)
          amplitude =
            (2 / windowSum)
              * sqrt ((leftReal * leftReal + leftImag * leftImag + rightReal * rightReal + rightImag * rightImag) / 2)
       in max (-90) $ 20 * logBase 10 (max 1.0e-9 amplitude)
   where
    frames =
      runGet
        (replicateM 2048 ((,) <$> getFloatle <*> getFloatle))
        (BL.fromStrict bytes)
    windowSum = sum [0.5 * (1 - cos (2 * pi * fromIntegral index / 2047)) | index <- [0 :: Int .. 2047]]

readProcess :: FilePath -> [String] -> String -> ExceptT String IO String
readProcess cmd args input =
  do
    (code, stdout, stderr) <-
      ExceptT $
        tryJust @IOException (\err -> Just $ show err) $
          Sys.readProcessWithExitCode cmd args input
    when (code /= ExitSuccess) $ throwE $ printf "%s failed with exit code %s: %s" cmd (show code) stderr
    pure stdout

-- | Get the MPD process ID
getMPDProcessId :: ExceptT String IO Int
getMPDProcessId = do
  res <- readProcess "systemctl" ["--user", "show", "mpd.service", "-p", "MainPID", "--value"] ""
  when (null res) $ throwE "Failed to get MPD process ID"
  ExceptT $ pure $ readEither res

-- | Get the MPD socket
getMPDSocket :: ExceptT String IO (SocketType, String, String)
getMPDSocket = do
  id' <- getMPDProcessId
  res <-
    catMaybes
      . fmap uncons
      . lines
      <$> readProcess "lsof" ["-Pan", "-a", "-p", show id', "-i", "-Ftn"] ""
  socket <- maybe (throwE "Failed to get MPD socket") pure $ lookup 'n' res
  ipType <- maybe (throwE "Failed to get MPD IP type") pure $ lookup 't' res
  let (ip, port) = break (== ':') socket
  pure (read ipType, ip, drop 1 port)

pipewireModuleTemplate :: String
pipewireModuleTemplate =
  $( do
       let fp = "assets" </> "pipewire" </> "meloid-eq.conf"
       addDependentFile fp
       content <- runIO (readFile fp)
       lift $ content
   )

{- | This function updates a module for the given audio server.
It creates the module directory if it does not exist.
It is currently only implemented for PipeWire.
-}
updateModuleEQId :: AudioServer -> String -> IO ()
updateModuleEQId PipeWire eqId = do
  homeDir <- getHomeDirectory
  configDir' <- getXdgDirectory XdgConfig "pipewire"
  let dir = configDir' </> "pipewire.conf.d"
  createDirectoryIfMissing True dir

  let str' = replace "%eqId%" eqId pipewireModuleTemplate
      str = replace "$HOME" homeDir str'
  writeFile (dir </> "meloid-eq.conf") str

-- | Restart the audio server.
restartAudioServer :: AudioServer -> ExceptT String IO ()
restartAudioServer PipeWire =
  void $
    readProcess "systemctl" ["--user", "restart", "pipewire", "pipewire-pulse", "wireplumber"] ""

-- | Restart the MPD server
restartMPDServer :: ExceptT String IO ()
restartMPDServer =
  -- run `systemctl --user restart mpd`
  void $
    readProcess "systemctl" ["--user", "restart", "mpd"] ""

-- | Extract extra information from a song
extractExtraInfo :: MPD.Song -> B.EventM (MName St) St (Either String SongFileExtraInfo)
extractExtraInfo MPD.Song{MPD.sgFilePath = path} = do
  musicDir <- use $ stConfig . csMusicDir
  liftIO $ runExceptT $ do
    path' <- resolveMusicFile musicDir (MPD.toString path)
    fileSize <- liftIO $ getFileSize path'
    stdout <-
      readProcess
        "ffprobe"
        [ "-v"
        , "error"
        , "-select_streams"
        , "a:0"
        , "-show_entries"
        , "stream=sample_rate,channels,bit_rate:format=bit_rate"
        , "-of"
        , "json"
        , path'
        ]
        ""
    decoded <-
      maybe
        (throwE "Failed to decode ffprobe output")
        pure
        (JSON.decode (BL8.pack stdout) :: Maybe JSON.Value)
    maybe
      (throwE "Bad ffprobe output")
      pure
      (parseSongFileExtraInfo fileSize decoded)

{- | Resolve an MPD-relative song path without allowing lexical
path traversal outside the configured music directory.
-}
resolveMusicFile :: FilePath -> FilePath -> ExceptT String IO FilePath
resolveMusicFile musicDir songPath
  | null musicDir = throwE "MPD music_directory is unavailable"
  | isAbsolute songPath = throwE "MPD returned an absolute song path"
  | otherwise = do
      root <- canonicalize musicDir
      let requested = normalise (root </> songPath)
      unless (isDescendantOf root requested) $
        throwE "MPD song path is outside music_directory"
      canonicalize requested
 where
  canonicalize path =
    ExceptT $
      tryJust @IOException (Just . displayException) (canonicalizePath path)

isDescendantOf :: FilePath -> FilePath -> Bool
isDescendantOf root path =
  let relative = makeRelative root path
   in relative /= "."
        && isRelative relative
        && ".." `notElem` splitDirectories relative

parseSongFileExtraInfo :: Integer -> JSON.Value -> Maybe SongFileExtraInfo
parseSongFileExtraInfo fileSize = \case
  JSON.Object root -> do
    JSON.Array streams <- JSON.lookup "streams" root
    (JSON.Object stream, _) <- Vec.uncons streams
    JSON.Object format <- JSON.lookup "format" root
    JSON.String sampleRate <- JSON.lookup "sample_rate" stream
    JSON.Number channels <- JSON.lookup "channels" stream
    JSON.String bitRate <- JSON.lookup "bit_rate" format
    sampleRate' <- readMaybe (Txt.unpack sampleRate)
    bitRate' <- readMaybe (Txt.unpack bitRate)
    pure $
      SongFileExtraInfo
        { songSize = formatBytes fileSize
        , songSampleRate = formatSampleRate sampleRate'
        , songChannels = case (Sci.floatingOrInteger channels :: Either Double Integer) of
            Left float -> show float
            Right int -> show int
        , songBitRate = formatBitrate bitRate'
        }
  _ ->
    Nothing
