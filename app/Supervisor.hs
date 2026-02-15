-- | Supervisor process: monitors child process and restarts on exit code 0.
{-# LANGUAGE TupleSections #-}

module Supervisor (supervise) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try, tryJust)
import System.IO.Error (isDoesNotExistError)
import Moe.Prelude
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.Posix.Signals
  ( Handler (..),
    installHandler,
    sigINT,
    sigTERM,
  )
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    ProcessHandle,
    StdStream (..),
    cleanupProcess,
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
  )

-- | Start supervisor mode: fork child with --supervised, restart on exit 0.
supervise :: IO ()
supervise = do
  exePath <- getExecutablePath
  userArgs <- getArgs

  -- Use MVar for thread-safe updates with proper synchronization
  childRef <- newMVar Nothing
  shutdownFlag <- newMVar False

  let shutdownHandler = do
        void $ swapMVar shutdownFlag True
        mChild <- swapMVar childRef Nothing
        -- Ignore "process does not exist" errors when terminating
        whenJust mChild $ \ph ->
          void $ tryJust (guard . isDoesNotExistError) (terminateProcess ph)

  _ <- installHandler sigTERM (Catch shutdownHandler) Nothing
  _ <- installHandler sigINT (Catch shutdownHandler) Nothing

  loop exePath ("--supervised" : userArgs) childRef shutdownFlag

-- | Main supervisor loop: spawn child, wait for exit, restart on code 0.
-- Uses bracket to ensure process cleanup on exceptions.
loop :: FilePath -> [String] -> MVar (Maybe ProcessHandle) -> MVar Bool -> IO ()
loop exePath args childRef shutdownFlag = do
  -- Check shutdown flag before starting new process
  shouldStop <- readMVar shutdownFlag
  when shouldStop $ exitWith (ExitFailure 130) -- Standard SIGINT exit code

  -- Note: Using putStrLn instead of Display instance because this is a standalone
  -- supervisor process that runs outside the effectful context. It must work
  -- independently of the application's effect system.
  putStrLn $ "[supervisor] Starting: " <> exePath
  hFlush stdout

  result <- try $ bracket
    (createProcess (proc exePath args){std_in = NoStream, std_out = Inherit, std_err = Inherit})
    (\(_, _, _, ph) -> cleanupProcess (Nothing, Nothing, Nothing, ph))
    $ \(_, _, _, ph) -> do
      void $ swapMVar childRef (Just ph)

      -- Double-check shutdown after setting childRef: if shutdown happened
      -- between createProcess and here, we need to terminate the new process.
      -- Use swap to claim ownership - if signal handler already took it, we get Nothing.
      isShuttingDown <- readMVar shutdownFlag
      when isShuttingDown $ do
        mChild <- swapMVar childRef Nothing
        whenJust mChild terminateProcess  -- Only terminate if we successfully claimed it

      -- Wait for process to exit (naturally or via termination above/from signal)
      exitCode <- waitForProcess ph
      -- Clear childRef if we still own it (signal handler might have cleared it already)
      void $ swapMVar childRef Nothing
      pure exitCode

  case (result :: Either SomeException ExitCode) of
    Left err -> do
      putStrLn $ "[supervisor] Process error: " <> show err
      exitWith (ExitFailure 1)
    Right ExitSuccess -> do
      putStrLn "[supervisor] Process exited with code 0, restarting in 1s..."
      hFlush stdout
      -- Use interruptible delay: check shutdown flag every 100ms
      restartDelay 10 shutdownFlag
      loop exePath args childRef shutdownFlag
    Right (ExitFailure code) -> do
      putStrLn $ "[supervisor] Process exited with code " <> show code <> ", stopping."
      hFlush stdout
      exitWith (ExitFailure code)

-- | Interruptible delay: sleep for n * 100ms, checking shutdown flag between each interval.
restartDelay :: Int -> MVar Bool -> IO ()
restartDelay 0 _ = pass
restartDelay n shutdownFlag = do
  shouldStop <- readMVar shutdownFlag
  if shouldStop
    then exitWith (ExitFailure 130)
    else do
      threadDelay 100_000 -- 100ms
      restartDelay (n - 1) shutdownFlag
