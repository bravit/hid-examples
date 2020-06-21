module ClientUtils (
    module DDefs,
    module RemoteIO,
    genClientDeclsFrom,
    callRemote) where

import DDefs
import Data.Serialize
import RemoteIO
import DeclsGenerator (genClientDeclsFrom)

callRemote :: (Serialize a, Serialize b) => Operation -> a -> RSIO st b
callRemote operation params = do
    sendRSIO (ReqCtx operation, encode params)
    answer <- receiveRSIO
    unEitherStaged Stage2 (decode answer)
