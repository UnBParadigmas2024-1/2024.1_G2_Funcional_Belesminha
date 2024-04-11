#!/bin/bash

OSTYPE=$(uname)

if [ "$OSTYPE" = "Darwin" ]; then
    brew uninstall cabal-install
    brew uninstall haskell-stack
elif [ "$OSTYPE" = "Linux" ]; then
    # Uninstall the libraries required for gloss
    sudo apt-get remove --purge cmake pkg-config -y
    sudo apt-get remove --purge mesa-utils libglu1-mesa-dev freeglut3-dev mesa-common-dev -y
    sudo apt-get remove --purge libglew-dev libglfw3-dev libglm-dev -y
    sudo apt-get remove --purge libao-dev libmpg123-dev -y
    sudo apt-get remove --purge cabal-install -y
    sudo apt-get remove --purge haskell-platform -y
    ghcup nuke
else
    echo "Windows"
fi

