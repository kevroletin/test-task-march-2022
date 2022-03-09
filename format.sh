#!/bin/sh

set -e
export LANG="C.UTF-8"

ormolu -m inplace $(find app -type f -name "*.hs")
ormolu -m inplace $(find src -type f -name "*.hs")
ormolu -m inplace $(find test -type f -name "*.hs")
