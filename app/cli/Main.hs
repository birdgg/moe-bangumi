module Main (main) where

import Options.Applicative

import Moe.Web.Server qualified as Server

newtype Command
  = Serve ServeOptions
  deriving stock (Show)

newtype ServeOptions = ServeOptions
  { port :: Word16
  }
  deriving stock (Show)

commandParser :: Parser Command
commandParser = Serve <$> serveOptionsParser

serveOptionsParser :: Parser ServeOptions
serveOptionsParser =
  ServeOptions
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 8080
          <> showDefault
          <> help "Port to listen on"
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
  Serve options <- execParser opts
  Server.runServer options.port
