module EchoList (main) where
import Ros.Node
import Lens.Family (view)
import qualified Ros.Std_msgs.String as S
import Ros.Topic.Util (fromList, toList)

procMsg :: S.String -> S.String
procMsg s =  S.String ("I heard " ++ S.__data s)


-- just to help with type inference. What is the analog of @arg in Coq,
-- to provide implcit arguments?
dummyID :: (Topic IO S.String) -> (Topic IO S.String)
dummyID x = x


toListN :: Topic IO a -> Node [a]
toListN t = liftIO (toList t)

main :: IO ()
main = runNode "echoer" $ do
    strmIn {-- (Topic IO S.String)--} <- (subscribe "chatter")
    lIn <- (toListN (dummyID strmIn)) -- This is what the Coq program will get as a CoList
    let lOut= (map procMsg lIn) in -- some processing in Coq
     advertise "chatterecho" (fromList lOut) -- realization of the publish function in Coq
	
