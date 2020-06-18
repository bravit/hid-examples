{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ClientUtils (
    module DDefs,
    module RemoteIO,
    callRemote) where

import DDefs
import Data.Serialize
import RemoteIO

callRemote :: (Serialize a, Serialize b) =>
                    Operation -> a -> RemoteStIO st b
callRemote operation params = do
    send (ReqCtx operation, encode params)
    answer <- receive
    decodeAnswerStage2 answer
    where
      decodeAnswerStage2 (ctx, res)
        | ok ctx = either (remoteError . const "Decoding error (stage 2)")  return (decode res)
        | otherwise = remoteError (excInfo ctx)

