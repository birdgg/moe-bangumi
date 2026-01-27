module Main (main) where

import Options.Applicative

data Command
  = Version
  | Sync SyncOptions
  deriving stock (Show)

newtype SyncOptions = SyncOptions
  { season :: Maybe Text
  }
  deriving stock (Show)

commandParser :: Parser Command
commandParser =
  subparser
    ( command "version" (info (pure Version) (progDesc "Show version"))
        <> command "sync" (info (Sync <$> syncOptionsParser) (progDesc "Sync bangumi data"))
    )

syncOptionsParser :: Parser SyncOptions
syncOptionsParser =
  SyncOptions
    <$> optional
      ( strOption
          ( long "season"
              <> short 's'
              <> metavar "SEASON"
              <> help "Anime season (e.g., '2026 Winter')"
          )
      )

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Moe Bangumi CLI"
        <> header "moe-cli - A Bangumi management tool"
    )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Version -> putTextLn "moe-bangumi 0.1.0.0"
    Sync options -> do
      putTextLn $ "Syncing with options: " <> show options
      putTextLn "Sync not implemented yet"
