# Design

## Code structure

Our mock simulates behavior or a "real" TrialChain:
* it validates tx signature;
* refuses double spend;
* refuses sending non existing money;
It also tracks some state:
* commited txs;
* account balances.
That logic is implemented in a Simulator (aka Sim).

The whole structure of is the server:
* Simulator consists of several functions operating on `SimState` (a pure state
  value);
* Servant-based server holds `SimState` using `TVar` and exposes simulator
  methods as RestAPI;
* there is `SimM` State/Except monad transformer to work with simulator methods.

Simulator:
* `TrialChain.Simulator` - monadic interface `SimM` using StateT and ExceptT;
* `TrialChain.Simulator.Pure` - simulator logic.

Server is implemented using Servant. All relevant definitions are in:
* `TrialChain.API`
* `TrialChain.Server`
* `TrialChain.Client`
* `TrialChain.Types`

Signing algorithm mock:
* `TrialChain.Signature`

## Design

1. Transaction structure

   Follows the task statement except for `nonce` field. We aren't implementing
   UTXO, and using account based tracking. `nonce` allows sending similar
   transactions multiple times.

2. Hash.

   * Serialize using derived Data.Binary instances.
   * Hash with cryptonite using md5.
   * Then convert to Base16 as requested.

   Normally serialization of data structures should be a specified part of the
   protocol. We just derive instances for simplicity. MD5 is a random choice.

3. Signature.

   Mocking signature using a trivial algorithm:
   * public key is any unicode string;
   * private key is the same string with "signed-by-" prefix;
   * signature is a string comprised of
     * a hash represented as Base16
     * with a privateKey appended as prefix.

   As a result signatures are quire readable which is good for manual debugging.

   Normally signatures and hashes would be represented inside of the app as
   ByteStrings. Show instances and serialization/deserialization might be
   implemented via conversion to readable ASCII symbols, such as as Base58 or
   others.

4. Http server.

   * Req/Resp encoding (TODO: explain)

   * Error Handling
     * all errors are reported by HTTP status code 400;
     * body of a response contains a string with a description of an error
       suitable for debugging.

   We follow a simple strategy for error handling. It's consistent, simple and
   sufficient for detecting and logging error for later investigation.

5. Storage.

   No storage.

6. Synchronization.

   Synchronize state updates using STM.

7. unit testing.

8. e2e testing.

9. performance testing.

10. coding style.
    + use ormolu - consistent, easy to use (but gives opinionated formatting)
    + prefer explicit imports (but it's ok not to import everything from Protolude).
