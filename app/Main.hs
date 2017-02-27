module Main where

import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import System.Posix.Files
import Options.Applicative
import Data.Semigroup

data Arguments = Arguments String String

data SocketState = Close | Exit deriving (Show, Eq)

opts :: Parser Arguments
opts = Arguments <$> strOption ( long "mode" <> short 'm' <> metavar "MODE" <> value "server" <> help "Select MODE for operation (server or client)" )
                 <*> strOption ( long "path" <> short 'p' <> value "/tmp/socket_test" <> help "Define the socket location" )

parseArgs :: ParserInfo Arguments
parseArgs = info (opts <**> helper) ( fullDesc <> progDesc "A UNIX socket server / client" <> header "Unleash the UNIX socket!" )

main :: IO ()
main = execParser parseArgs >>= run

run :: Arguments -> IO ()
run (Arguments mode socketPath) = case mode of
  "server" -> runServer socketPath
  "client" -> runClient socketPath
  _ -> error $ "Unrecognized run mode " <> mode

runServer :: FilePath -> IO ()
runServer socketPath = withSocketsDo $ do
       sock <- socket AF_UNIX Stream 0
       bind sock (SockAddrUnix socketPath)
       listen sock maxListenQueue
       go sock
       exit sock

    where
      go :: Socket -> IO ()
      go sock = do
        result <- talk =<< accept sock
        case result of
          Close -> go sock
          Exit -> return ()

      talk :: (Socket, SockAddr) -> IO SocketState
      talk connection@(conn, _) = do
        msg <- recv conn 16384
        if C.unpack msg == ":close" then do
          echo msg conn
          return Close
        else if C.unpack msg == ":quit" then do
          echo msg conn
          return Exit
        else if C.null msg then do
          putStrLn "here"
          echo (C.pack "Null") conn
          talk connection
        else do
          echo msg conn
          talk connection

      echo msg conn = do
        putStr "Received: "
        C.putStrLn msg
        reply conn $ C.unpack msg

      reply :: Socket -> String -> IO ()
      reply conn msg = sendAll conn $ C.pack msg

      exit :: Socket -> IO ()
      exit conn = do
        close conn
        removeLink socketPath
        putStrLn "Done"

runClient :: FilePath -> IO ()
runClient socketPath = withSocketsDo $ do
  go socketPath
  putStrLn "Done"

  where
    go :: FilePath -> IO ()
    go socketPath = do
      prompt
      sock <- socket AF_UNIX Stream 0
      connect sock (SockAddrUnix socketPath)
      chat sock socketPath

    chat :: Socket -> FilePath -> IO ()
    chat sock socketPath = do
      msg <- getLine
      sendAll sock $ C.pack msg
      msg <- recv sock 1024
      if C.unpack msg == ":quit"
        then do
          putStrLn "Quit received. Shutting down."
          close sock
          return ()
        else if C.unpack msg == ":close"
          then do
            putStrLn "Close received. Re-connecting new socket."
            go socketPath
        else do
          putStr "Response: "
          C.putStrLn msg
          chat sock socketPath

    prompt :: IO ()
    prompt = do
      putStrLn "\n\nWelcome to socket client!"
      putStrLn "-------------------------"
      putStrLn "\n:quit to quit the program"
      putStrLn ":close to close the existing socket connection"
      putStrLn "\nType your input below:\n"
