module ClientUtils (
    module RpcCommon,
    module RemoteIO,
    remote,
    callRemote) where

import RpcCommon
import Data.Serialize
import RemoteIO
import DeclsGenerator (remote)

callRemote :: (Serialize a, Serialize b) => Operation -> RemoteAction st a b
callRemote operation params = do
    sendRSIO (operation, encode params)
    answer <- receiveRSIO
    unEitherStaged Stage2 (decode answer)
