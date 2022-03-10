# Design

## Goals

From the conversation, email and the task statement we conclude the main is
demonstrating ability to **write maintainable code by**:
+ documenting
+ using known tools
+ setting up testing, building, CI

Non goals:
+ demonstrating ability to invent/implement complex algorithm

## Missing requirements

tldr;
+ no performance considerations;
+ no timing simulation, tx finalization happens instantly;
+ no failure injection;

The inspiration for the task is writing a testing mock. But we don't know what
kind of testing we are supposed to do. Important missing requirements are:
1. Performance. Stress testing real-world system would stress-test our mock. For
   that reason we might think about using efficient way of storing large amount
   of transitions. For simplicity we use simple in-memory storage.

2. Simulating timings. In the real world committing a transition takes time and
   might fail. Bitcoin easily takes 10+ minutes to write a transition into the
   blockchain and it takes more time to wait for additional confirmations. We
   don't simulate this behavior for simplicity.

3. Failure injection. Real-world systems fail and mocks might also be configured
   to return failures. We skip this hypothetical feature.

4. Restoring testing data. Since we don't think about performance, we assume
   that testing data can be inserted using our mock API.

## Design

1. Transaction structure

   + No UTXO, account based tracking, hence Tx has nonce field, so we can send
     similar transactions many times

2. Hash.

   + Serialize using derived Data.Binary instances.
   + Hash with cryptonite using md5.
   + Then convert to Base16 as requested.

3. Signature.

   Mocking signature using a trivial algorithm:
   + public key is any string
   + private key is the same string with "signed-by-" prefix
   + signature is hash represented as Base16 with a privateKey as prefix.

   As a result signatures are quire readable which is good for debugging.

   Normally signatures and hashes would be represented inside of the app as
   ByteStrings. Show instances and serialization/deserialization might be
   implemented via conversion to readable ASCII symbols, such as as Base58 or
   others.

4. Http server.

   + Req/Resp encoding (TODO: explain)

   + Error Handling
     - TODO: error names
     - all errors are returned with 400 code (TODO: explain)

5. Storage.
6. Synchronization.
7. unit testing.
8. e2e testing.
9. performance testing.
10. coding style.
    + just use ormolu - consistent, easy to use (but gives opinionated formatting)

