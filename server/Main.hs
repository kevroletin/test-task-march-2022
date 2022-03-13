import Control.Concurrent.STM (newTVarIO)
import Network.Wai.Handler.Warp (run)
import Protolude
import TrialChain.Server (ServerState (..), trialChainApp)
import TrialChain.Simulator (mkState)
import TrialChain.Types (PublicKey (..))

main :: IO ()
main = do
  storage <- newTVarIO $ mkState [(PublicKey "bank", 99999999999999)]
  run 8081 (trialChainApp (ServerState storage))
