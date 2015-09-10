module Echo2List (main) where
import Ros.Node
import Lens.Family (view)
import qualified Ros.Std_msgs.String as S
import qualified Ros.Std_msgs.Int8 as RI
import Ros.Topic.Util (asapMerge)
import Ros.Topic (mapEitherL,mapEitherR)
import Ros.ROSCoqUtil

type EchMsg = Either S.String RI.Int8

int2str:: RI.Int8 -> S.String
int2str i =  (S.String ("I Heard num " ++ show i))


main :: IO ()
main = runNode "echoer" $ do
    strmInL {--(Topic IO S.String)--} <- subscribeCoList "chatter"
    strmInR {--(Topic IO RI.Int8)--}  <- subscribeCoList "chatterInt"
    strmOut <- liftIO $ asapMerge  strmInL (map int2str strmInR)
    publishCoList "chatterecho" (strmOut)

