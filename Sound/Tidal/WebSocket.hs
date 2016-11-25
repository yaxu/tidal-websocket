{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Sound.Tidal.WebSocket where

import Control.Exception (try)
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Tidal.Stream as Tidal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.List
import Data.Ratio
import Data.Maybe
import Control.Concurrent
import System.Cmd
import Data.Time.Clock.POSIX
import Data.Fixed (mod')

import Sound.Tidal.Hint
-- import Sound.Tidal.Draw

import Control.Concurrent.MVar

import Control.Applicative
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromRow as SFR

type TidalState = (Int,
                   [ThreadId],
                   String -> IO (),
                   Tidal.ParamPattern -> IO(),
                   MVar [(Int, Tidal.ParamPattern)],
                   (MVar String, MVar Response),
                   S.Connection,
                   MVar (Tidal.Tempo),
                   Double -> IO ()
                  )

data ChangeField = ChangeField Int T.Text deriving (Show)

instance SFR.FromRow ChangeField where
  fromRow = ChangeField <$> S.field <*> S.field

instance S.ToRow ChangeField where
  toRow (ChangeField id_ str) = S.toRow (id_, str)

port = 9162

wsSend :: WS.Connection -> IO (ThreadId, String -> IO())
wsSend conn =
  do sendQueue <- newEmptyMVar
     threadId <- (forkIO $ sender sendQueue)
     return (threadId, putMVar sendQueue)
  where
    sender :: MVar String -> IO ()
    sender sendQueue = do s <- takeMVar sendQueue
                          WS.sendTextData conn (T.pack s)
                          sender sendQueue

cpsUtils'' :: IO ((Double -> IO (), Double -> IO (), IO (Rational), MVar (Tidal.Tempo)))
cpsUtils'' = do (mTempo, mCps, mNudge) <- Tidal.runClient
                let cpsSetter b = putMVar mCps b
                    nudger = putMVar mNudge
                    currentTime = do tempo <- readMVar mTempo
                                     now <- Tidal.beatNow tempo
                                     return $ toRational now
                return (cpsSetter, nudger, currentTime, mTempo)

run = do
  putStrLn $ "TidalCycles websocket server, starting on port " ++ show port
  mPatterns <- newMVar []
  mConnectionId <- newMVar 0
  (cps, nudger, getNow, mTempo) <- cpsUtils''
  -- hack - give clock server time to warm up before connecting to it
  threadDelay 500000
  cps 0.85
  (d,_) <- Tidal.dirtSetters getNow
  -- d <- Tidal.dirtStream
  mIn <- newEmptyMVar
  mOut <- newEmptyMVar
  forkIO $ hintJob (mIn, mOut)
  sql <- S.open "test.db"
  -- S.execute_ sql "DROP TABLE cx"
  -- S.execute_ sql "DROP TABLE change"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS cx (name TEXT)"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS change (cxid INTEGER, json TEXT)"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS snapshot (cxid INTEGER, json TEXT)"
  WS.runServer "0.0.0.0" port $ (\pending -> do
    conn <- WS.acceptRequest pending
    putStrLn $  "received new connection"
    (senderThreadId, sender) <- wsSend conn
    S.execute sql "INSERT INTO cx (name) VALUES (?)" (S.Only ("anon" :: String))
    cxid <- fromIntegral <$> S.lastInsertRowId sql

    -- putStrLn $ "pat count: " ++ show (length pats)
    -- putStrLn "modified mvar"

    sender $ "/welcome " ++ show cxid
    
    pats <- takeMVar mPatterns
    putMVar mPatterns ((cxid, Tidal.silence):pats)
    
    WS.forkPingThread conn 30
    clockThreadId <- (forkIO $ Tidal.clockedTick 4 (onTick sender))
    let state = (cxid, [senderThreadId,clockThreadId], sender, d, mPatterns, (mIn, mOut), sql, mTempo, nudger)
    loop state conn
    )
  putStrLn "done."

onTick :: (String -> IO ()) -> Tidal.Tempo -> Int -> IO ()
onTick sender tempo tick = do forkIO $ do (threadDelay $ floor ((Tidal.latency Tidal.dirt) * 1000000))
                                          sender ("/bang " ++ show tick)
                              return ()

loop :: TidalState -> WS.Connection -> IO ()
loop state conn = do
  putStrLn "loop"
  msg <- try (WS.receiveData conn)
  -- add to dictionary of connections -> patterns, could use a map for this
  case msg of
    Right s -> do
      act state conn (T.unpack s)
      loop state conn
    Left WS.ConnectionClosed -> close state "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close state "by request from peer"
    Left (WS.ParseException e) -> close state ("parse exception: " ++ e)

close :: TidalState -> String -> IO ()
close (cxid,threadIds,_,d,mPatterns,_,_,_,_) msg = do
  pats <- takeMVar mPatterns
  let pats' = filter ((/= cxid) . fst) pats
      ps = map snd pats'
  putMVar mPatterns pats'
  d $ Tidal.stack ps
  mapM_ killThread threadIds
  putStrLn ("connection closed: " ++ msg)

-- hush = mapM_ ($ Tidal.silence)

-- TODO: proper parsing..
takeNumbers :: String -> (String, String)
takeNumbers xs = (takeWhile f xs, dropWhile (== ' ') $ dropWhile f xs)
  where f x = not . null $ filter (x ==) "0123456789."

act :: TidalState -> WS.Connection -> String -> IO ()
act state@(cxid,_,sender,d,mPatterns,(mIn,mOut),sql,mTempo,nudger) conn request
  | isPrefixOf "/eval " request =
    do putStrLn (show request)
       let (when, code) = takeNumbers $ fromJust $ stripPrefix "/eval " request
       putMVar mIn code
       r <- takeMVar mOut
       case r of OK p -> do putStrLn "updating"
                            updatePat state (conn, p)
                            t <- (round . (* 100)) `fmap` getPOSIXTime
                            let fn = "/home/alex/SparkleShare/embedded/print/" ++ show t
                            --drawText (fn ++ ".pdf") code (Tidal.dirtToColour p)
                            -- rawSystem "convert" [fn ++ ".pdf", fn ++ ".png"]
                            putStrLn "updated"
                            sender $ "/eval " ++ when ++ " " ++ code
                 Error s -> sender $ "/error " ++ when ++ " " ++ s
       return ()
  | isPrefixOf "/panic" request =
    do putStrLn (show request)
       swapMVar mPatterns []
       d $ Tidal.silence
  | isPrefixOf "/shutdown" request =
    do rawSystem "sudo" ["halt"]     
       return ()
  | isPrefixOf "/change " request =
      do S.execute sql "INSERT INTO change (cxid,json) VALUES (?,?)" (ChangeField cxid (T.pack $ fromJust $ stripPrefix "/change " request))
         return ()
  | isPrefixOf "/pulse " request =
    do let diff = (read $ takeWhile ((flip elem) ("-01234567890." :: String)) $ fromJust $ stripPrefix "/pulse " request) :: Double
       t <- readMVar mTempo
       -- seconds per cycle
       let spc = 1 / (Tidal.cps t)
           diff' | diff > (spc/2) = diff - spc
                 | diff < (0-(spc/2)) = diff + spc
                 | otherwise = diff
       putStrLn $ "nudging: " ++ show diff' ++ " (not " ++ show diff ++ ")"
       nudger diff'
       return ()

act _ _ _ = return ()

updatePat :: TidalState -> (WS.Connection, Tidal.ParamPattern) -> IO ()
updatePat (cxid, _, _, d, mPatterns,_,_,_,_) (conn, p) =
  do pats <- takeMVar mPatterns
     let pats' = ((cxid,p) : filter ((/= cxid) . fst) pats)
         ps = map snd pats'
     -- putStrLn $ show ps
     putMVar mPatterns pats'
     d $ Tidal.stack ps
     return ()
     
