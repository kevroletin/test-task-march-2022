import Control.Concurrent.STM
import Network.Wai.Handler.Warp
import Protolude
import TrialChain.AppState
import TrialChain.Server
import TrialChain.Types

main :: IO ()
main = do
  storage <- newTVarIO $ mkState [(PublicKey "bank", 99999999999999)]
  run 8081 (trialChainApp (ServerState storage))
