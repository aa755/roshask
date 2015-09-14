module Echo (main) where
import Ros.Node
import Lens.Family (view)
import Ros.ROSCoqUtil
import qualified Ros.Std_msgs.String as S
import  Control.Concurrent


showMsg :: S.String -> IO ()
showMsg = putStrLn . ("I heard " ++) . view S._data


-- just to help with type inference. What is the analog of @arg in Coq,
-- to provide implcit arguments?
dummyID :: (Topic IO S.String) -> (Topic IO S.String)
dummyID x = x


publishStrmDelayedOnChan :: (Topic IO S.String) -> (Chan S.String) -> Node ()
publishStrmDelayedOnChan t c = do
   (x,t') <- liftIO $ runTopic t
   _ <- publishMsgOnChan  c x
   publishStrmDelayedOnChan t' c
   

main :: IO ()
main = runNode "echoer" $ do
    strmIn {-- Node (Topic IO S.String)--} <- subscribe "chatter"
    _ <- liftIO $ putStrLn "subscribed"
    c <- advertiseNewChan "chatterecho"
    _ <- liftIO $ putStrLn "advertized"
    publishStrmDelayedOnChan strmIn c

