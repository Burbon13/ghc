module DictParamIgnored where

applyDouble :: Num a => a -> a
applyDouble x = x + x

boringFunc = applyDouble 10

-- Add dict parameter (ignored, only parsed)
-- interestingFunc = applyDouble @{whatever} 10
