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

data TidalState = TidalState {cxid :: Int,
                              clients :: [ThreadId],
                              sender :: String -> IO (),
                              dirt :: Tidal.ParamPattern -> IO(),
                              mPatterns :: MVar [(Int, Tidal.ParamPattern)],
                              mIn :: MVar String,
                              mOut :: MVar Response,
                              sql :: S.Connection,
                              mTempo :: MVar (Tidal.Tempo),
                              nudger :: Double -> IO (),
                              cps :: Double -> IO ()
                             }

data ChangeField = ChangeField Int T.Text deriving (Show)
data EvalField   = EvalField   Int T.Text deriving (Show)

instance SFR.FromRow ChangeField where
  fromRow = ChangeField <$> S.field <*> S.field

instance S.ToRow ChangeField where
  toRow (ChangeField id_ str) = S.toRow (id_, str)

instance SFR.FromRow EvalField where
  fromRow = EvalField <$> S.field <*> S.field

instance S.ToRow EvalField where
  toRow (EvalField id_ str) = S.toRow (id_, str)

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
  cps 0.65
  -- (d,_) <- Tidal.dirtSetters getNow
  (d,_) <- Tidal.superDirtSetters getNow
  -- d <- Tidal.dirtStream
  mIn <- newEmptyMVar
  mOut <- newEmptyMVar
  forkIO $ hintJob (mIn, mOut)
  sql <- S.open "test.db"
  -- S.execute_ sql "DROP TABLE cx"
  -- S.execute_ sql "DROP TABLE change"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS cx (name TEXT)"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS change (cxid INTEGER, json TEXT)"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS eval (cxid INTEGER, code TEXT)"
  S.execute_ sql "CREATE TABLE IF NOT EXISTS snapshot (cxid INTEGER, json TEXT)"
  WS.runServer "0.0.0.0" port $ (\pending -> do
    conn <- WS.acceptRequest pending

    putStrLn $  "received new connection"
    (senderThreadId, sender) <- wsSend conn

    S.execute sql "INSERT INTO cx (name) VALUES (?)" (S.Only ("anon" :: String))
    cxid <- fromIntegral <$> S.lastInsertRowId sql

    sender $ "/welcome " ++ show cxid
    
    pats <- takeMVar mPatterns
    putMVar mPatterns ((cxid, Tidal.silence):pats)
    
    WS.forkPingThread conn 30
    clockThreadId <- (forkIO $ Tidal.clockedTick 4 (onTick sender))
    let state = TidalState cxid [senderThreadId,clockThreadId] sender d mPatterns mIn mOut sql mTempo nudger cps
    loop state conn
    )
  putStrLn "done."

onTick :: (String -> IO ()) -> Tidal.Tempo -> Int -> IO ()
onTick sender tempo tick = do forkIO $ do (threadDelay $ floor ((Tidal.latency Tidal.dirt) * 1000000))
                                          sender ("/bang " ++ show tick)
                              return ()

loop :: TidalState -> WS.Connection -> IO ()
loop state conn = do
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
close ts msg = do
  pats <- takeMVar (mPatterns ts)
  let pats' = filter ((/= (cxid ts)) . fst) pats
      ps = map snd pats'
  putMVar (mPatterns ts) pats'
  dirt ts $ Tidal.stack ps
  mapM_ killThread (clients ts)
  putStrLn ("connection closed: " ++ msg)

-- hush = mapM_ ($ Tidal.silence)

-- TODO: proper parsing..
takeNumbers :: String -> (String, String)
takeNumbers xs = (takeWhile f xs, dropWhile (== ' ') $ dropWhile f xs)
  where f x = not . null $ filter (x ==) "0123456789."

commands = [("play", act_play) ,
            ("panic", act_panic) {-,
            ("shutdown", act_shutdown),
            ("change", act_change),
            ("nudge", act_nudge),
            ("cps_delta ", act_cps_delta),
            ("cps", act_cps),
            ("bang", act_bang True),
            ("nobang", act_bang False)-}
           ]

getCommand :: String -> Maybe (TidalState -> WS.Connection -> IO ())
getCommand ('/':s) = do f <- lookup command commands
                        param <- stripPrefix command s
                        let param' = dropWhile (== ' ') param
                        return $ f param'
  where command = takeWhile (/= ' ') s
getCommand _ = Nothing

act :: TidalState -> WS.Connection -> String -> IO ()
--act state@(cxid,_,sender,d,mPatterns,(mIn,mOut),sql,mTempo,nudger,cps) conn request
act ts conn request = (fromMaybe act_no_parse $ getCommand request) ts conn

act_no_parse ts conn = sender ts $ "/noparse"

act_play :: String -> TidalState -> WS.Connection -> IO ()
act_play param ts conn = 
  do 
    putMVar (mIn ts) param
    r <- takeMVar (mOut ts)
    case r of OK p -> do updatePat ts (conn, p)
                         --t <- (round . (* 100)) `fmap` getPOSIXTime
                         --let fn = "/home/alex/SparkleShare/embedded/print/" ++ show t
                         -- drawText (fn ++ ".pdf") code (Tidal.dirtToColour p)
                         -- rawSystem "convert" [fn ++ ".pdf", fn ++ ".png"]
                         sender ts $ "/eval " ++ param
                         S.execute (sql ts) "INSERT INTO eval (cxid,code) VALUES (?,?)" (EvalField (cxid ts) (T.pack param))
              Error s -> sender ts $ "/error " ++ s
    return ()

act_panic :: String -> TidalState -> WS.Connection -> IO ()
act_panic param ts conn = 
  do swapMVar (mPatterns ts) []
     dirt ts $ Tidal.silence
{-
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
       nudger (diff' + (Tidal.latency Tidal.dirt))
       return ()
  | isPrefixOf "/faster" request =    
    do t <- readMVar mTempo
       cps $ (Tidal.cps t) + 0.01
       return ()
  | isPrefixOf "/slower" request =    
    do t <- readMVar mTempo
       cps $ (Tidal.cps t) - 0.01
       return ()

act _ _ _ = return ()
-}

updatePat :: TidalState -> (WS.Connection, Tidal.ParamPattern) -> IO ()
updatePat ts (conn, p) =
  do pats <- takeMVar (mPatterns ts)
     let pats' = ((cxid ts,p) : filter ((/= (cxid ts)) . fst) pats)
         ps = map snd pats'
     -- putStrLn $ "updating pattern: " ++ show (Tidal.stack ps)
     putMVar (mPatterns ts) pats'
     dirt ts $ Tidal.stack ps
     return ()
     
