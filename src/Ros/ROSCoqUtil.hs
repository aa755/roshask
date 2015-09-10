{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}

-- | These functions are specializations of roshask functions, intended to serve as realizers of the API exported to Coq
-- via extraction.


module Ros.ROSCoqUtil (nbind, nreturn, toListN, publishCoList , subscribeCoList, asapMergeCoList) where
import Ros.Topic.Util (fromList, toList)
import Ros.Node
import Ros.Internal.RosBinary (RosBinary)
import Ros.Internal.Msg.MsgInfo
import Data.Typeable.Internal
import Control.Concurrent

nreturn::a  -> Node a
nreturn x = return x

asapMerge :: [a] ->  [a] -> IO [a]
asapMerge t1 t2 =  do
                         c <- newChan
                         _ <- forkIO $ writeList2Chan c t1
                         _ <- forkIO $ writeList2Chan c t2
                         getChanContents c

asapMergeCoList :: [a] ->  [a] -> Node [a]
asapMergeCoList t1 t2 =  liftIO (asapMerge t1 t2)

toListN :: Topic IO a -> Node [a]
toListN t = liftIO (toList t)

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



