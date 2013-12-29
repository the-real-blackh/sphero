{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
import Network.Protocol.Orbotix.Sphero
import Control.Monad.Trans (liftIO)
import FindSphero


main :: IO ()
main = do
    s <- findSphero
    runSphero s $ do
        v <- getVersioning
        liftIO $ print v
