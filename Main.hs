module Main where
import Sound.Tidal.WebSocket
import Control.Concurrent

-- I read somewhere that comms with the 'main' thread was slow, or something.
main = waitForkIO $ Sound.Tidal.WebSocket.run

waitForkIO :: IO () -> IO ()
waitForkIO io = do
    mvar <- newEmptyMVar
    forkFinally io (\_ -> putMVar mvar ())
    takeMVar mvar
