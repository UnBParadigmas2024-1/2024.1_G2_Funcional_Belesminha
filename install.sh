#! /bin/bash

#!/bin/bash

OSTYPE=$(uname)

if [ $OSTYPE = "Darwin" ];
    then
        brew install haskell-stack;
        brew install cabal-install;
        cabal install --lib gloss;
elif [ $OSTYPE = "Linux "];
    then
        echo "OI"
fi
