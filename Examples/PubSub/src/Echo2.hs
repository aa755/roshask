module Echo2 (main) where
import Ros.Node
import Lens.Family (view)
import qualified Ros.Std_msgs.String as S

showMsg :: S.String -> IO ()
showMsg = putStrLn . ("I heard " ++) . view S._data


-- just to help with type inference. What is the analog of @arg in Coq,
-- to provide implcit arguments?
dummyID :: (Topic IO S.String) -> (Topic IO S.String)
dummyID x = x

main :: IO ()
main = runNode "echoer" $ do
    strmIn {-- Node (Topic IO S.String)--} <- subscribe "chatter"
    advertise "chatterecho" (dummyID strmIn)

