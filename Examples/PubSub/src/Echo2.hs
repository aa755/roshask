module Echo2 (main) where
import Ros.Node
import Lens.Family (view)
import qualified Ros.Std_msgs.String as S
import qualified Ros.Std_msgs.Int8 as RI
import Ros.Topic.Util ((<+>))
import Ros.Topic (mapEitherL,mapEitherR)


type EchMsg = Either S.String RI.Int8

procMsg:: EchMsg -> EchMsg
procMsg (Left s) = Left (S.String ("I Heard " ++ show s))
procMsg (Right v) = Right (RI.Int8 (RI.__data v+ RI.__data v))


main :: IO ()
main = runNode "echoer" $ do
    strmInL {--(Topic IO S.String)--} <- subscribe "chatter"
    strmInR {--(Topic IO RI.Int8)--}  <- subscribe "chatInt"
    let strmLR = strmInL <+> strmInR
        strmOut = fmap procMsg strmLR 
	strmOutL = mapEitherL id strmOut
	strmOutR = mapEitherR id strmOut in do	
		_ <- (advertise "chatterecho" (strmOutL))
		advertise "chatterIntecho" (strmOutR)

