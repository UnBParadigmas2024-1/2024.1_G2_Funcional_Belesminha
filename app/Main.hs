import Graphics.Gloss

import World (mazeToPicture,handleInput,updateWorld,World(..),initializeWorld)

main :: IO ()
main = do
  let newWorld = initializeWorld
      minStepsPath = minSteps newWorld
  play
    (InWindow "Belesminha" (1000, 1000) (0, 0))
    black
    60
    newWorld
    (mazeToPicture minStepsPath)
    handleInput
    updateWorld