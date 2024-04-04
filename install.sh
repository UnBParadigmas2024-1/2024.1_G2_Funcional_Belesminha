#!/bin/bash

OSTYPE=$(uname)

if [ "$OSTYPE" = "Darwin" ]; then
    brew install haskell-stack
    brew install cabal-install
elif [ "$OSTYPE" = "Linux" ]; then
    sudo apt-get update
    sudo apt-get install haskell-platform
    sudo apt-get install cabal-install
else
    echo "Windows"
fi

if [ "$OSTYPE" = "Linux" ] || [ "$OSTYPE" = "Darwin" ]; then
    cabal update
    cabal install --lib gloss
    cabal install --lib random
fi