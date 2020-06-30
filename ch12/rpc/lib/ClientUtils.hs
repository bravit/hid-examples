module ClientUtils (
    module RpcCommon,
    module RemoteIO,
    remote,
    callRemote) where

import RpcCommon
import Data.Serialize
import RemoteIO
import DeclsGenerator (remote)

callRemote :: (Serialize a, Serialize b) => Operation -> a -> RSIO st b
callRemote operation params = do
    sendRSIO (ReqCtx operation, encode params)
    answer <- receiveRSIO
    unEitherStaged Stage2 (decode answer)
