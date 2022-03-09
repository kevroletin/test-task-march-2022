# trial-chain-api

## Install

## Build

## Run

## Test

## Coding style

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
