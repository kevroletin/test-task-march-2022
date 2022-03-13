## Scope

From the conversation, email and the task statement we conclude the main is
demonstrating ability to **write maintainable code by**:
+ designing sane API
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
