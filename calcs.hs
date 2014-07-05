module Main where

aref_voltage = 3.3

-- | convert from Fahrenheight to the analog millivolt voltage reading
c2v :: Double -> Double
c2v c = (c / 100) + 0.5

-- | convert from Fahrenheight to Celcius
f2c :: Double -> Double
f2c f = (5/9) * (f - 32)

-- | convert voltage to analog sample value (0-1023)
v2a :: Double -> Double
v2a v = (v * 1024) / aref_voltage


-- | convert Fahrenheight to analog sample value
f2a :: Double -> Double
f2a = v2a . c2v . f2c
