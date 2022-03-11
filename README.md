# Trial Chain mock

In addition to the [design](doc/design.md) doc, there are also:

* original [problem statement](doc/Coinweb_Haskell_Task.pdf);
* simplified [stripped task defenition](doc/task.md);
* [scope](doc/scope.md) - our discussion that we aren't implementing anything complicated;

## Build

```
stack build
```

## Run

Important notes:
* no logging
* hardcoded **port 8081**
* both server and test start with **hardcoded magic "bank" account** which contains
  large quantity of money for testing;

```
stack run server-exe
```

You can make sure the server is running using curl:

```
curl http://localhost:8081/alive
```

## Test

Unit tests and api tests:

```
stack test
```

## Manual testing

You can generate signed transactions by using ghci:

```
$ stack repl src/TrialChain/Signature.hs

import Data.Aeson
putStrLn . encode $ _testSignTx "bank" "pedro" 100 ""

{"body": ...
```

And send requests by REST client of your choice:

```
PUT http://localhost:8081/tx
Content-type: application/json

{"body":{"from":"bank","to":"pedro","amount":100,"nonce":""},"signature":"signed-by-bank-90d2e36e37adaaa5fbe854f77b02837c"}
```

## Profiling

Done manually. Build sever with profiling, run load-test, analyze profiles
manually.

Hunting down memory leaks:

```
stack run --profile server-exe -- +RTS  -hy -l-agu -RTS
eventlog2html server-exe.eventlog && firefox server-exe.eventlog.html
```

Hunting down performance bottlenecks:

```
stack run --profile server-exe -- +RTS -p -RTS
ghc-prof-flamegraph server-exe.prof && firefox firefox server-exe.svg
```

Running a tester.

```
stack run load-test-exe
```

## Coding style

Prefer explicit imports.
Format using `ormolu`. Make sure `ormolu` is runnable and execute `sh ./format.sh`.

A hack to copy tools from a current workspace into stack's global bin directory and add it to PATH:
```
copy-stack-tools() {
    stack build --copy-compiler-tool hlint hoogle weeder hindent ghcid ormolu .
}

stackify() {
    compiler_bin=$(stack path --compiler-bin)
    compiler_tools_bin=$(stack path --compiler-tools-bin)
    if [ ! -d $compiler_tools_bin ]; then
        copy-stack-tools
    fi
    if [ ! -d $compiler_bin ]; then
        stack build --install-ghc
    fi
    PATH=$compiler_bin:$compiler_tools_bin:$PATH
}
```
