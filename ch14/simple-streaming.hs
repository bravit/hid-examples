{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Data.Bifunctor
import Data.Functor.Compose
import Control.Monad

data Stream f m r = Step (f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r

empty :: Stream f m ()
empty = Return ()

effect :: Monad m => m r -> Stream f m r
effect eff = Effect $ Return <$> eff

data Of a b = a :> b
  deriving Show

instance Functor (Of a) where
  fmap f (a :> b) = a :> f b

instance Bifunctor Of where
  bimap f g (a :> b) = f a :> g b

yield :: a -> Stream (Of a) m ()
yield a = Step (a :> empty)

each :: [e] -> Stream (Of e) m ()
each [] = Return ()
each (x:xs) = Step $ x :> each xs

ssum :: (Num e, Monad m) => Stream (Of e) m r -> m (Of e r)
ssum (Return r) = pure (0 :> r)
ssum (Effect m) = m >>= ssum
ssum (Step (e :> str)) = first (+e) <$> ssum str

printStream :: (Show e, Show r) => Stream (Of e) IO r -> IO ()
printStream (Return r) = putStrLn $ "Result: " <> show r
printStream (Effect m) = do
  putStrLn "Run action:"
  str <- m
  printStream str
printStream (Step (e :> str)) = do
  putStrLn $ "Element: " <> show e
  printStream str

instance (Functor f, Monad m) => Functor (Stream f m) where
  fmap :: forall a b. (a -> b) -> Stream f m a -> Stream f m b
  fmap fun stream = loop stream
    where
      loop :: Stream f m a -> Stream f m b
      loop (Return r) = Return (fun r)
      loop (Effect m) = Effect (fmap loop m)
      loop (Step f) = Step (fmap loop f)

instance (Functor f, Monad m) => Monad (Stream f m) where
  (>>=) :: forall r r1.
           Stream f m r -> (r -> Stream f m r1) -> Stream f m r1
  stream >>= fun = loop stream
    where
      loop :: Stream f m r -> Stream f m r1
      loop (Return r) = fun r
      loop (Effect m) = Effect (fmap loop m)
      loop (Step f) = Step (fmap loop f)

instance (Functor f, Monad m) => Applicative (Stream f m) where
  pure r = Return r
  (<*>) = ap

stream1 :: Stream (Of Int) IO ()
stream1 = do
  yield 1
  effect (putStrLn "action 1")
  yield 2
  effect (putStrLn "action 2")
  yield 3


maps :: (Functor f, Monad m) => (forall x. f x -> g x) -> Stream f m r -> Stream g m r
maps fun = loop
  where
    loop (Return r) = Return r
    loop (Effect m) = Effect (fmap loop m)
    loop (Step f) = Step (fun (fmap loop f))

mapOf :: Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
mapOf fun = maps (first fun)

zipsWith :: Monad m
  => (forall x y p . (x -> y -> p) -> f x -> g y -> h p)
  -> Stream f m r -> Stream g m r -> Stream h m r
zipsWith fun = loop
  where
    loop (Return r) _ = Return r
    loop _ (Return r) = Return r
    loop (Effect m) t = Effect $ fmap (flip loop t) m
    loop s (Effect n) = Effect $ fmap (loop s) n
    loop (Step fs) (Step gs) = Step $ fun loop fs gs


zipPair :: Monad m =>
           Stream (Of a) m r -> Stream (Of b) m r -> Stream (Of (a, b)) m r
zipPair = zipsWith fun
  where
    fun p (e1 :> x) (e2 :> y) = (e1, e2) :> (p x y)

zips :: (Monad m, Functor f, Functor g)
     => Stream f m r -> Stream g m r -> Stream (Compose f g) m r
zips = zipsWith fun
  where
    fun p fx gy = Compose (fmap (\x -> fmap (\y -> p x y) gy) fx)



decompose :: (Monad m, Functor f) =>
             Stream (Compose m f) m r -> Stream f m r
decompose = loop
  where
    loop (Return r) = Return r
    loop (Effect m) = Effect (fmap loop m)
    loop (Step (Compose mstr)) = Effect $ do
      str <- mstr
      pure (Step (fmap loop str))



mapsM :: (Monad m, Functor f, Functor g) =>
         (forall x . f x -> m (g x)) -> Stream f m r -> Stream g m r
mapsM fun = decompose . maps (Compose . fun)

withEffect :: Monad m =>
              (a -> m ()) -> Stream (Of a) m r -> Stream (Of a) m r
withEffect eff = mapsM go
  where
    go p@(e :> _) = eff e >> pure p

withPrinting :: Show a => Stream (Of a) IO r -> Stream (Of a) IO r
withPrinting = withEffect (\e -> putStrLn ("Element: " ++ show e))

splitsAt :: (Monad m, Functor f) =>
            Int -> Stream f m r -> Stream f m (Stream f m r)
splitsAt = loop
  where
    loop n stream
     | n > 0 =
         case stream of
           Return r -> Return (Return r)
           Effect m -> Effect (fmap (loop n) m)
           Step f  -> Step (fmap (loop (n-1)) f)
     | otherwise = Return stream

chunksOf :: forall f m r. (Monad m, Functor f) =>
            Int -> Stream f m r -> Stream (Stream f m) m r
chunksOf n = loop
  where
    cutChunk :: Stream f m r -> Stream f m (Stream (Stream f m) m r)
    cutChunk str = fmap loop (splitsAt (n-1) str)

    loop :: Stream f m r -> Stream (Stream f m) m r
    loop (Return r) = Return r
    loop (Effect m) = Effect (fmap loop m)
    loop (Step fs) = Step (Step (fmap cutChunk fs))

main :: IO ()
main = do
  s :> () <- ssum $ withEffect print $ mapsM ssum $ chunksOf 2
            $ each [1,1,1,1,1 :: Int]
  print s
