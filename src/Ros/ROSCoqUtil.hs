{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}

-- | These functions are specializations of roshask functions, intended to serve as realizers of the API exported to Coq
-- via extraction.


module Ros.ROSCoqUtil (nbind, nreturn, publishCoList, subscribeCoList, asapMergeCoList, advertiseNewChan, publishMsgOnChan, publishDelayedMsgOnChan, coFoldLeft, flattenTL) where
import Ros.Topic.Util (fromList, toList)
import Ros.Node
import Ros.Internal.RosBinary (RosBinary)
import Ros.Internal.Msg.MsgInfo
import Data.Typeable.Internal
import Control.Concurrent

nreturn::a  -> Node a
nreturn x = return x

asapMergeL :: [a] ->  [a] -> IO [a]
asapMergeL t1 t2 =  do
                         c <- newChan
                         _ <- forkIO $ writeList2Chan c t1
                         _ <- forkIO $ writeList2Chan c t2
                         getChanContents c

asapMergeCoList :: [a] ->  [a] -> Node [a]
asapMergeCoList t1 t2 =  liftIO (asapMergeL t1 t2)

toListN :: Topic IO a -> Node [a]
toListN t = liftIO (toList t)


lcons :: Monad m => [a] -> Topic m a -> m (a, Topic m a)
lcons [] t = runTopic t
lcons (h:tl) t = return (h, Topic (lcons tl t))

flattenTL :: Topic IO [a] -> Topic IO a
flattenTL t = Topic $ do 
    (x, t') <- runTopic t
    lcons x (flattenTL t')     

nbind:: Node a -> (a -> Node b) -> Node b
nbind x f =  x >>= f

-- | When Coq types are generated for ROS message, this function will be specialized to that message type.
-- TopicName is just String. If this changes in upstream, the Coq API and this function will have to be updated.
subscribeCoList::(RosBinary a, MsgInfo a, Typeable a) => TopicName -> Node [a]
subscribeCoList s =
    do
       t <- (subscribe s)
       toListN t

-- | When Coq types are generated for ROS message, this function will be specialized to that message type.
-- TopicName is just String. If this changes in upstream, the Coq API and this function will have to be updated.
publishCoList::(RosBinary a, MsgInfo a, Typeable a) => TopicName -> [a] -> Node ()
publishCoList s l = advertise s (fromList l)

-- |advertise a new channel for publishing on a given topic. one can write messages 
-- to the returned channel to publish them. This provides a way to avoid using the
-- stream passing style when it seems cumbersome.
advertiseNewChan ::(RosBinary a, MsgInfo a, Typeable a) => TopicName -> Node (Chan a)
advertiseNewChan name = do
                         c <- liftIO newChan
                         cc <- liftIO $ getChanContents c
                         _ <- publishCoList name cc
                         return c

publishMsgOnChan::(Chan a) -> a -> Node ()
publishMsgOnChan c m = liftIO $ writeChan c m

-- | The documentation of threadDelay says that it works only for GHC. 
publishDelayedMsgOnChanAux::Int -> (Control.Concurrent.Chan a) -> a -> Node ()
publishDelayedMsgOnChanAux micros c m = do
     _ <- liftIO $ threadDelay micros
     publishMsgOnChan c m

-- | FIX!! either maked the delay bounded in the Coq type signature, or using a loop
-- properly handle the case when the first argument is beyond the bounds of Int
publishDelayedMsgOnChan::Prelude.Integer -> (Control.Concurrent.Chan a) -> a -> Node ()
publishDelayedMsgOnChan micros c m = 
     publishDelayedMsgOnChanAux (Prelude.fromInteger micros) c m
     


coFoldLeft::(a-> b ->Node a)-> [b] -> a -> Node a
coFoldLeft _ [] inita = return inita
coFoldLeft f (h:tl) inita = do
    ha <- (f inita h)
    coFoldLeft f tl ha
