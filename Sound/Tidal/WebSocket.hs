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

import Sound.Tidal.Hint
import Control.Concurrent.MVar

type TidalState = (Int, Tidal.ParamPattern -> IO(), MVar [(Int, Tidal.ParamPattern)])

port = 9162

main = do
  -- putStrLn $ "TidalCycles websocket server, listening on port " ++ show port
  mPatterns <- newMVar []
  mConnectionId <- newMVar 0
  (cps, getNow) <- Tidal.bpsUtils
  (d,_) <- Tidal.superDirtSetters getNow
  WS.runServer "0.0.0.0" port $ (\pending -> do
    conn <- WS.acceptRequest pending
    putStrLn $  "received new connection"
    -- putStrLn $ "pat count: " ++ show (length pats)
    -- putStrLn "modified mvar"
    cid <- takeMVar mConnectionId
    let cid' = cid + 1
    putMVar mConnectionId cid'

    WS.sendTextData conn (T.pack $ "/welcome " ++ show cid)
    
    pats <- takeMVar mPatterns
    putMVar mPatterns ((cid, Tidal.silence):pats)
    
    WS.forkPingThread conn 30
    loop (cid, d, mPatterns) conn
    )

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
close (cid,d,mPatterns) msg = do
  pats <- takeMVar mPatterns
  let pats' = filter ((/= cid) . fst) pats
      ps = map snd pats'
  putMVar mPatterns pats'
  d $ Tidal.stack ps
  putStrLn ("connection closed: " ++ msg)
  -- hush dss

-- hush = mapM_ ($ Tidal.silence)

act :: TidalState -> WS.Connection -> String -> IO ()
act state@(cid,d,mPatterns) conn request
  | isPrefixOf "/eval " request =
  do putStrLn (show request)
     let code = fromJust $ stripPrefix "/eval " request
     r <- runJob code
     case r of OK p -> do WS.sendTextData conn (T.pack "good.")
                          putStrLn "updating"
                          updatePat state (conn, p)
                          putStrLn "updated"
                          WS.sendTextData conn (T.pack "looping..")
               Error s -> WS.sendTextData conn (T.pack $ "bad: " ++ s)
     return ()
  | isPrefixOf "/panic" request =
  do putStrLn (show request)
     swapMVar mPatterns []
     d $ Tidal.silence
     

act _ _ _ = return ()

updatePat :: TidalState -> (WS.Connection, Tidal.ParamPattern) -> IO ()
updatePat (cid, d, mPatterns) (conn, p) =
  do pats <- takeMVar mPatterns
     putStrLn $ show $ length pats
     let pats' = ((cid,p) : filter ((/= cid) . fst) pats)
         ps = map snd pats'
     putStrLn "right then."
     putStrLn $ show $ length pats'
     putStrLn $ show ps
     putMVar mPatterns pats'
     d $ Tidal.stack ps
     return ()
     
{-
processRequest (_,dss) (Pattern n p) = do
  x <- hintParamPattern p
  case x of (Left error) -> do
              putStrLn "Error interpreting pattern"
              return Nothing
            (Right paramPattern) -> do
              dss!!(n-1) $ paramPattern
              return Nothing

processRequest _ (Render patt cps cycles) = do
  x <- hintParamPattern patt
  case x of (Left error) -> do
              putStrLn "Error interpreting pattern"
              return Nothing
            (Right paramPattern) -> do
              let r = render paramPattern cps cycles
              putStrLn (encodeStrict r)
              return (Just (showJSON r))
-}
