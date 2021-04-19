{-# LANGUAGE OverloadedStrings #-}

module EvalRPNExcept where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import TextShow
import Data.Foldable (traverse_)

data EvalError = NotEnoughElements
               | ExtraElements
               | NotANumber Text
               | UnknownVar Text

instance TextShow EvalError where
  showb NotEnoughElements = "Not enough elements in the expression"
  showb ExtraElements = "There are extra elements in the expression"
  showb (NotANumber t) = "Expression component '" <>
                         fromText t <> "' is not a number"
  showb (UnknownVar t) = "Variable '" <>
                         fromText t <> "' not found"

type Stack = [Integer]

type EnvVars = [(Text, Integer)]

type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = get >>= pop'
  where
    pop' :: Stack -> EvalM Integer
    pop' [] = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  len <- gets length
  when (len /= 1) $ throwError ExtraElements

readVar :: Text -> EvalM Integer
readVar name = do
  var <- asks (lookup name)
  case var of
    Just n -> pure n
    Nothing -> throwError $ UnknownVar name

readNumber :: Text -> EvalM Integer
readNumber txt =
  case decimal txt of
    Right (n, rest) | T.null rest -> pure n
    _ -> throwError $ NotANumber txt

readSafe :: Text -> EvalM Integer
readSafe t
  | isId t = readVar t
  | otherwise = readNumber t
  where
    isId txt = maybe False (isLetter . fst) (T.uncons txt)

evalRPNOnce ::Text -> EvalM Integer
evalRPNOnce str =
    clearStack >> traverse_ step (T.words str) >> oneElementOnStack >> pop
  where
    clearStack = put []
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push

evalRPNMany :: [Text] -> EnvVars -> Text
evalRPNMany txts env = reportEvalResults $
    evalState (runExceptT (runReaderT (mapM evalOnce txts) env)) []
  where
    evalOnce txt = (fromText txt <>) <$>
      (buildOk <$> evalRPNOnce txt) `catchError` (pure . buildErr)
    buildOk res = " = " <> showb res
    buildErr err = " Error: " <> showb err

reportEvalResults :: Either EvalError [Builder] -> Text
reportEvalResults (Left e) = "Error: " <> showt e
reportEvalResults (Right b) = toText $ unlinesB b
