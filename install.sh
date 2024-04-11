#!/bin/bash

OSTYPE=$(uname)

if [ "$OSTYPE" = "Darwin" ]; then
    brew install haskell-stack
    brew install cabal-install
elif [ "$OSTYPE" = "Linux" ]; then
    sudo apt-get update -y
    
    # Essas bibliotecas garantem que o OpenGL no sistema esteja de acordo com os requisitos para instalar o gloss
    sudo apt-get install cmake pkg-config -y
    sudo apt-get install mesa-utils libglu1-mesa-dev freeglut3-dev mesa-common-dev -y
    sudo apt-get install libglew-dev libglfw3-dev libglm-dev libgl-dev -y
    sudo apt-get install libao-dev libmpg123-dev -y

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
    export PATH="$HOME/.ghcup/bin:$PATH"
    ghcup install ghc 9.8.2
    ghcup install cabal 3.10.3.0

    echo "alias cabal='cabal-3.10.3.0'" >> ~/.bashrc 
    echo "alias ghc='ghc-9.8.2'" >> ~/.bashrc 
    source ~/.bashrc
else
    echo "Windows"
fi

if [ "$OSTYPE" = "Linux" ] || [ "$OSTYPE" = "Darwin" ]; then
    bash -c "cabal update"
fi