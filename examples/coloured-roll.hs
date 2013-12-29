{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
import Network.Protocol.Orbotix.Sphero
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import FindSphero


main :: IO ()
main = do
    s <- findSphero
    runSphero s $ do
        forkSphero $ forever $ do
            forever $ do
                forM_ [0,5..359] $ \h -> do
                    let (r, g, b) = hsv_to_rgb (h :: Double) 1 1
                    color (toW r) (toW g) (toW b)
                    liftIO $ threadDelay 20000
        let sp = 40
        forever $ do
            roll sp 0
            liftIO $ threadDelay 2000000
            roll sp 90
            liftIO $ threadDelay 2000000
            roll sp 180
            liftIO $ threadDelay 2000000
            roll sp 270
            liftIO $ threadDelay 2000000
  where
    toW a = floor $ a * 255

-- | hue is in degrees
hsv_to_rgb :: (Num a, Fractional a, RealFloat a) => a -> a -> a -> (a, a, a)
hsv_to_rgb h s v = case hi of
    0 -> (v,t,p)
    1 -> (q,v,p)
    2 -> (p,v,t)
    3 -> (p,q,v)
    4 -> (t,p,v)
    _ -> (v,p,q)
 where
  hi = floor (h/60) `mod` 6
  f = mod1 (h/60)
  p = v*(1-s)
  q = v*(1-f*s)
  t = v*(1-(1-f)*s)

  mod1 x | pf < 0 = pf+1
         | otherwise = pf
     where
      (_,pf) = properFraction x

