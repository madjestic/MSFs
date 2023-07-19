{-# LANGUAGE QuasiQuotes #-}
import Text.Interpolation.Nyan
--import PyF

main = 
  let who = "world"
  in [int||Hello #{who}!|]

