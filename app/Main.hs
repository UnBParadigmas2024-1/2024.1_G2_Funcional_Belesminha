import Graphics.Gloss
import World (mazeToPicture,handleInput,updateWorld,World(..),initializeWorld, returnCurrentWindow)

main :: IO ()
main = do
  let newWorld = initializeWorld
      minStepsPath = minSteps newWorld
  play
    (returnCurrentWindow newWorld)
    black
    60
    newWorld
    (mazeToPicture minStepsPath)
    handleInput
    updateWorld