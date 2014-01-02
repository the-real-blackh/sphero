{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
import Network.Protocol.Orbotix.Sphero
import Network (withSocketsDo)
import Control.Monad.Trans (liftIO)
import FindSphero


main :: IO ()
main = withSocketsDo $ do
    s <- findSphero
    runSphero s $ do
        v <- getVersioning
        liftIO $ print v
