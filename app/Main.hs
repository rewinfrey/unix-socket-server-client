module Main where

import Control.Monad
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import System.Posix.Files
import Options.Applicative
import Data.Semigroup
import Control.Monad.Trans.State

data Arguments = Arguments String String

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
        (conn,_) <- accept sock
        talk conn
        go sock

      talk :: Socket -> IO ()
      talk conn =
          do msg <- recv conn 16384
             if C.null msg
               then reply conn "Nothing received!"
               else if C.unpack msg == ":quit" then do
                 reply conn "Quitting!"
                 putStrLn "Shutting down..."
                 exit conn
               else do
                 C.putStrLn $ C.pack "\n"
                 C.putStrLn msg
                 reply conn $ C.unpack msg

      reply :: Socket -> String -> IO ()
      reply conn msg = pure () -- sendAll conn $ C.pack msg

      exit :: Socket -> IO ()
      exit conn = do
        close conn
        removeLink socketPath
        putStrLn "DONE"

runClient :: FilePath -> IO ()
runClient socketPath = withSocketsDo $ do
  sock <- socket AF_UNIX Stream 0
  connect sock (SockAddrUnix socketPath)
  putStrLn "Please type your message:"
  chat sock

  where
    chat sock = do
      msg <- getLine
      sendAll sock $ C.pack msg
      msg <- recv sock 1024
      putStr "Received "
      C.putStrLn msg
      chat sock
