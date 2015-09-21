{-# LANGUAGE TemplateHaskell #-}
module IcreateDriver (main) where
import Prelude hiding (dropWhile)
import Control.Applicative
import Control.Arrow
import System.IO (hFlush, stdout)
import Data.Complex
import Ros.Logging
import Ros.Node
import Ros.Topic (repeatM, force, dropWhile, metamorphM, yieldM)
import Ros.Topic.Util (everyNew, interruptible, forkTopic, topicOn, subsample)
import Ros.Util.PID (pidTimedIO)
import Ros.Geometry_msgs.Twist
import qualified Ros.Geometry_msgs.Vector3 as V
import GHC.Base

mkLinVel ::  Prelude.Double -> V.Vector3
mkLinVel x  = V.Vector3 x 0 0

mkAngularVel ::  Prelude.Double -> V.Vector3
mkAngularVel z  = V.Vector3  0 0 z


-- Create Twist message with linear and angular velocity in arguments.
mkTwist :: Prelude.Double -> Prelude.Double -> Twist
mkTwist lin rot = Twist (mkLinVel lin) (mkAngularVel rot)

mkTwistV :: V.Vector3 -> Twist
mkTwistV v = mkTwist (V._x v) (V._y v)

units :: Topic GHC.Base.IO ()
units = repeatM (return ())

units10Hz :: Topic IO ()
units10Hz = (topicRate 10 units)

postProc :: Topic IO (V.Vector3, ()) -> Topic IO Twist
postProc = fmap (\p -> mkTwistV (fst p))

-- | unlike the new driver for icreate, the last message does not need to be repeated at 10hz when using this driver.
-- Unlike the old drivers from Brown Univ, this one makes the robot stop if no message is recieved for 1/10 sec.
-- This file can be considered a haskell equiv. of https://github.com/aa755/ROSCoq/blob/05c097f2ed4f53f4721b4b3b1775f9902d9841ac/src/shim/oldJavaShim/ICreateDriverNoRep.java

main = runNode "ICreateSimplifiedDriver" $
       do enableLogging (Just Warn)
          coqCmds <- subscribe "/icreate_cmd_vel"
          let merged = everyNew coqCmds units10Hz
          advertise "/mobile_base/commands/velocity" $ postProc merged
