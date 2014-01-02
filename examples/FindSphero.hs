module FindSphero where

import qualified Data.ByteString as B
import Network.Bluetooth

findSphero :: IO RFCOMMSocket
findSphero = do
    mAdapter <- defaultAdapter
    adapter <- case mAdapter of
        Just adapter -> return adapter
        Nothing      -> fail $ "Can't find a local bluetooth adapter"
    devs <- discover adapter
    case filter isSphero devs of
        (dev:_) -> openRFCOMM dev 1
        []      -> fail $ "Can't find a sphero device"

isSphero :: Device -> Bool
isSphero (Device _ (BluetoothAddr addr)) = B.pack [0x66, 0x06, 0x00] `B.isSuffixOf` addr
