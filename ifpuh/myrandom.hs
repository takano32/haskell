#!/usr/bin/env hugs
import Random

main = do x <- getStdGen
          print (fst (next x))
          newStdGen
          x <- getStdGen
          print (next x)
          newStdGen
          x <- getStdGen
          print (next x)


