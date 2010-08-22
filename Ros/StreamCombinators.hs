{-# LANGUAGE BangPatterns #-}
module Ros.StreamCombinators where
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import qualified Data.Foldable as F
import Ros.Stream (Stream(..))
import qualified Ros.Stream as S
import System.IO.Unsafe (unsafeInterleaveIO)

-- |Turn a stream of values into a stream of pairs of consecutive
-- values.
consecutive :: Stream a -> Stream (a,a)
consecutive s = (,) <$> s <*> S.tail s

-- |Return pairs of items from two streams advancing through the
-- streams in lockstep. If data is being generated by one stream much
-- faster than the other, this is a bad fit.
lockstep :: Stream a -> Stream b -> Stream (a,b)
lockstep s1 s2 = (,) <$> s1 <*> s2

-- |Stream a new pair every time either of the component 'Stream's
-- produces a new value. The value of the other element of the pair
-- will be the newest available value. The resulting 'Stream' will
-- produce a new value at the rate of the faster component 'Stream',
-- and may contain duplicate consecutive elements.
everyNew :: Stream a -> Stream b -> IO (Stream (a,b))
everyNew s t = warmup <$> (s <|> t)
    where warmup (Cons (Left x) xs) = 
              let Cons (Right y) ys = S.dropWhile isLeft xs
              in Cons (x,y) $ product x y ys
          warmup (Cons (Right y) ys) = 
              let Cons (Left x) xs = S.dropWhile isRight ys
              in Cons (x,y) $ product x y xs
          product _ y (Cons (Left x') xs) = Cons (x',y) $ product x' y xs
          product x _ (Cons (Right y') ys) = Cons (x,y') $ product x y' ys
          isLeft x = case x of { Left _ -> True; _ -> False }
          isRight x = case x of { Right _ -> True; _ -> False }

-- |Stream a new pair every time both of the component 'Stream's have
-- produced a new value. The composite 'Stream' will produce pairs at
-- the rate of the slower component 'Stream' consisting of the most
-- recent value from each 'Stream'.
bothNew :: Stream a -> Stream b -> IO (Stream (a,b))
bothNew xs ys = go Nothing Nothing <$> xs <|> ys
    where go (Just x) _ (Cons (Right y) ys) = Cons (x,y) $ go Nothing Nothing ys
          go _ (Just y) (Cons (Left x) xs) = Cons (x,y) $ go Nothing Nothing xs
          go _ Nothing (Cons (Left x) xs) = go (Just x) Nothing xs
          go Nothing _ (Cons (Right y) ys) = go Nothing (Just y) ys

infixl 7 <|>
-- |Merge two 'Stream's into one. Items from each component 'Stream'
-- will be tagged with an 'Either' constructor and added to the
-- combined 'Stream' as they become available.
(<|>) :: Stream a -> Stream b -> IO (Stream (Either a b))
s <|> t = S.fromList <$> mergeIO (map Left (S.toList s)) (map Right (S.toList t))

-- |Merge two 'Stream's into one. The items from each component
-- 'Stream' will be added to the combined 'Stream' as they become
-- available.
merge :: Stream a -> Stream a -> IO (Stream a)
merge s t = fmap extract <$> s <|> t
    where extract (Left x) = x
          extract (Right x) = x

-- |Apply a function to each consecutive pair of elements from a
-- 'Stream'.
finiteDifference :: (a -> a -> b) -> Stream a -> Stream b
finiteDifference f s = fmap (uncurry f) $ consecutive s
{-# INLINE finiteDifference #-}

-- |Perform numerical integration of a 'Stream' using Simpson's rule
-- applied at three consecutive points. This requires a function for
-- adding values from the 'Stream', and a function for scaling values
-- by a fractional number.
simpsonsRule :: Fractional n => 
                (a -> a -> a) -> (n -> a -> a) -> Stream a -> Stream a
simpsonsRule plus scale s = go s
    where go stream = Cons (simpson (S.take 3 stream)) (go (S.tail stream))
          c = 1 / 6
          simpson [a,mid,b] = scale c $ plus (plus a (scale 4 mid)) b
          simpson _ = error "Impossible pattern in simpson"
{-# INLINE simpsonsRule #-}

-- |Compute a running \"average\" of a 'Stream' by summing the product
-- of @alpha@ and the current average with the product of @1 - alpha@
-- and the newest value. The first parameter is the constant @alpha@,
-- the second is an addition function, the third a scaling function,
-- and the fourth the input 'Stream'.
weightedMean :: Num n => 
                n -> (a -> a -> a) -> (n -> a -> a) -> Stream a -> Stream a
weightedMean alpha plus scale = weightedMean2 alpha (1 - alpha) plus scale
{-# INLINE weightedMean #-}

-- |Compute a running \"average\" of a 'Stream' by summing the product
-- of @alpha@ and the current average with the product of @invAlpha@
-- and the newest value. The first parameter is the constant @alpha@,
-- the second is the constant @invAlpha@, the third is an addition
-- function, the fourth a scaling function, and the fifth the input
-- 'Stream'.
weightedMean2 :: n -> n -> (a -> a -> a) -> (n -> a -> a) -> Stream a -> Stream a
weightedMean2 alpha invAlpha plus scale = warmup
    where warmup (Cons x xs) = go x xs
          go avg (Cons x xs) = let !savg = scale alpha avg
                                   !sx = scale invAlpha x
                                   !avg' = plus savg sx
                               in Cons avg' (go avg' xs)
{-# INLINE weightedMean2 #-}

-- |Compute a running \"average\" of a 'Stream' using a user-provided
-- normalization function applied to the sum of products. The
-- arguments are a constat @alpha@ that is used to scale the current
-- average, a constant @invAlpha@ used to scale the newest value, a
-- function for adding two scaled values, a function for scaling
-- input values, a function for normalizing the sum of scaled values,
-- and finally the stream to average. Parameterizing over all the
-- arithmetic to this extent allows for the use of denormalizing
-- scaling factors, as might be used to keep all arithmetic
-- integral. An example would be scaling the average by the integer
-- 7, the new value by the integer 1, then normalizing by dividing
-- the sum of scaled values by 8.
weightedMeanNormalized :: n -> n -> (b -> b -> c) -> (n -> a -> b) -> 
                          (c -> a) -> Stream a -> Stream a
weightedMeanNormalized alpha invAlpha plus scale normalize = warmup
    where warmup (Cons x xs) = go x xs
          go avg (Cons x xs) = let !avg' = normalize $ plus (scale alpha avg)
                                                            (scale invAlpha x)
                               in Cons avg' (go avg' xs)
{-# INLINE weightedMeanNormalized #-}

-- |Use a 'Stream' of functions to filter a 'Stream' of values. Each
-- function is applied to the second 'Stream' until it returns
-- 'True'. At that point, 'filterBy' produces the accepted value of
-- the second 'Stream' and moves on to the next function which is
-- applied to the rest of the second 'Stream'.
filterBy :: Stream (a -> Bool) -> Stream a -> Stream a
filterBy (Cons f fs) xs = Cons x $ filterBy fs xs'
    where Cons x xs' = S.dropWhile (not . f) xs

-- |Produce elements of the first 'Stream' no faster than elements of
-- the second 'Stream' become available.
gate :: Stream a -> Stream b -> Stream a
gate = curry $ fmap fst . uncurry lockstep

-- |Flatten a 'Stream' of 'F.Foldable' values. For example, turn a
-- @Stream [a]@ of finite lists into a @Stream a@ by taking each
-- element from each list in sequence.
concats :: F.Foldable f => Stream (f a) -> Stream a
concats (Cons x xs) = F.foldr Cons (concats xs) x

-- |Flatten a 'Stream' of 'F.Foldable' values such that old values are
-- discarded as soon as the original 'Stream' produces a new
-- 'F.Foldable'.
interruptible :: F.Foldable t => Stream (t a) -> IO (Stream a)
interruptible s = 
    do feeder <- newEmptyMVar         -- Active feeder thread
       latestItem <- newEmptyMVar     -- Next available item
       let feedItems ys = do ft <- tryTakeMVar feeder
                             maybe (return ()) killThread ft
                             t <- forkIO $ F.traverse_ (putMVar latestItem) ys
                             putMVar feeder t 
           watchForItems s = let !nxt = S.head s
                             in feedItems nxt >> 
                                watchForItems (S.tail s)
           getAll = do x <- unsafeInterleaveIO $ takeMVar latestItem
                       xs <- unsafeInterleaveIO getAll
                       return $ Cons x xs
       _ <- forkIO $ watchForItems s
       getAll
