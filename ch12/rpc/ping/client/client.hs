import ClientUtils
import RFunctions

import System.CPUTime
import Control.Monad.Trans

makePing :: RemoteStIO () (String, Integer)
makePing = do
    t1 <- liftIO getCPUTime
    res <- ping
    t2 <- liftIO getCPUTime
    let t = t2-t1
    pure (res, t)

makePing2 :: RemoteStIO Integer ()
makePing2 = do
  ping2 >>= liftIO . print
  ping2 >>= liftIO . print
  ping2 >>= liftIO . print
  ping2 >>= liftIO . print

instance RemoteState Integer where
    initState = 0

main :: IO ()
main = runRemote (PeerAddr "localhost" 1500) makePing2 >> pure ()

