FROM ubuntu:22.04 AS build_stage
RUN apt-get update -y && apt-get upgrade -y
RUN apt-get install ghc cabal-install haskell-stack haskell-platform -y

RUN apt-get install cmake pkg-config -y
RUN apt-get install mesa-utils libglu1-mesa-dev freeglut3-dev mesa-common-dev -y
RUN apt-get install libglew-dev libglfw3-dev libglm-dev libgl-dev -y
RUN apt-get install libao-dev libmpg123-dev -y
RUN apt-get install g++ -y

WORKDIR /app
COPY ./app ./app
COPY ./x20241-G2-Funcional-Belesminha.cabal x20241-G2-Funcional-Belesminha.cabal

RUN cabal update
RUN cabal build

FROM alpine:latest AS app
WORKDIR /build
COPY --from=build_stage /app/dist-newstyle ./dist-newstyle