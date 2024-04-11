#!/bin/bash

OSTYPE=$(uname)

if [ "$OSTYPE" = "Darwin" ]; then
    brew install haskell-stack
    brew install cabal-install
elif [ "$OSTYPE" = "Linux" ]; then
    sudo apt-get update -y
    sudo apt-get install haskell-platform -y
    sudo apt-get install cabal-install -y
    
    # Essas bibliotecas garantem que o OpenGL no sistema esteja de acordo com os requisitos para instalar o gloss
    sudo apt-get install cmake pkg-config -y
    sudo apt-get install mesa-utils libglu1-mesa-dev freeglut3-dev mesa-common-dev -y
    sudo apt-get install libglew-dev libglfw3-dev libglm-dev -y
    sudo apt-get install libao-dev libmpg123-dev -y
else
    echo "Windows"
fi

if [ "$OSTYPE" = "Linux" ] || [ "$OSTYPE" = "Darwin" ]; then
    cabal update
    sudo cabal build
fi