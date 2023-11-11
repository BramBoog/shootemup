module Model.Randomness where
import Model.Parameters

import System.Random (randomRIO)

generateProbability :: IO Double
generateProbability = randomRIO (0.0, 1.0)

randomY :: Float -> IO Float
randomY border = randomRIO (screenMinY + border, screenMaxY - border)

newtype ProbDist a = ProbDist [(Double, a)] deriving Show

normalize :: ProbDist a -> ProbDist a
normalize (ProbDist was) = ProbDist (map (\(p, a) -> (p / sum (map fst was), a)) was)

chooseWithProb :: ProbDist a -> IO a
chooseWithProb pd = let ProbDist was = normalize pd
                      in do r <- generateProbability
                            return (chooseFromList r was)
  where
    chooseFromList :: Double -> [(Double, a)] -> a
    chooseFromList _ [] = error "The generated value was greater than all the probability brackets, or the provided distribution was empty."
    chooseFromList p ((w, a) : was') = if p <= w then a
                                        else chooseFromList (p - w) was'
