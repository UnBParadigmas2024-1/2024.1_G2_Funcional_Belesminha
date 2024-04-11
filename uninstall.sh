#!/bin/bash

OSTYPE=$(uname)

if [ "$OSTYPE" = "Linux" ] || [ "$OSTYPE" = "Darwin" ]; then
    cabal uninstall gloss
    cabal uninstall random
    cabal uninstall containers
fi

if [ "$OSTYPE" = "Darwin" ]; then
    brew uninstall haskell-stack
    brew uninstall cabal-install
elif [ "$OSTYPE" = "Linux" ]; then
    sudo apt-get remove --purge haskell-platform -y
    sudo apt-get remove --purge cabal-install -y
    
    # Uninstall the libraries required for gloss
    sudo apt-get remove --purge cmake pkg-config -y
    sudo apt-get remove --purge mesa-utils libglu1-mesa-dev freeglut3-dev mesa-common-dev -y
    sudo apt-get remove --purge libglew-dev libglfw3-dev libglm-dev -y
    sudo apt-get remove --purge libao-dev libmpg123-dev -y
else
    echo "Windows"
fi

