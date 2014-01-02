{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
-- | Client library for Orbotix Sphero.
--
-- See examples directory for an example.
module Network.Protocol.Orbotix.Sphero (
        Sphero,
        runSphero,
        forkSphero,
        ping,
        getVersioning,
        Versioning(..),
        color,
        roll
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize
import Data.Typeable
import Data.Word
import GHC.Generics
import Network.Bluetooth
import Numeric


data Command = Ping
             | GetVersioning
             | Roll { roSpeed :: Word8, roHeading :: Int }
             | Color { coRed :: Word8, coGreen :: Word8, coBlue :: Word8, coSave :: Bool }
    deriving (Generic, Show)

instance Serialize Command where

data Reply = SimpleResponse Word8 ByteString
           | PowerNotification ByteString
           | Level1Diag ByteString
           | SensorData ByteString
           | ConfigBlock ByteString
           | PreSleepWarning ByteString
           | MacroMarkers ByteString
           | CollisionDetection ByteString
           | OrbPrint ByteString
           | OrbErrorASCII ByteString
           | OrbErrorBinary ByteString
    deriving (Generic, Show)

data Packet a = Packet Word8 a
    deriving (Generic, Show)

instance Serialize (Packet Command) where
    put (Packet seq Ping) = addCK $ do
        putWord8 0
        putWord8 1
        putWord8 seq
        putWord8 1

    put (Packet seq GetVersioning) = addCK $ do
        putWord8 0
        putWord8 2
        putWord8 seq
        putWord8 1

    put (Packet seq (Roll sp hding)) = addCK $ do
        let hding' = fromIntegral (hding `mod` 360)
        putWord8 2
        putWord8 0x30
        putWord8 seq
        putWord8 5
        putWord8 sp
        putWord16be hding'
        putWord8 0xff

    put (Packet seq (Color r g b s)) = addCK $ do
        putWord8 2
        putWord8 0x20
        putWord8 seq
        putWord8 5
        putWord8 r
        putWord8 g
        putWord8 b
        putWord8 $ if s then 0xff else 0

instance Serialize Reply where
    get = do
        sop1 <- getWord8
        unless (sop1 == 0xff) $ fail $ "unexpected SOP1 byte 0x"++showHex sop1 ""
        sop2 <- getWord8
        case sop2 of
            0xff -> do
                mrsp <- getWord8
                seq <- getWord8
                len <- getWord8
                dta <- getByteString (fromIntegral (len - 1))
                ck <- getWord8
                -- to do: check ck
                case mrsp of
                    0x00 -> pure $ SimpleResponse seq dta
                    _    -> fail $ "Unknown packet received 0x"++showHex mrsp ""
            0xfe -> do
                idCode <- getWord8
                dlen <- getWord16be
                dta <- getByteString (fromIntegral (dlen - 1))
                ck <- getWord8
                -- to do: check ck
                case idCode of
                    0x01 -> pure $ PowerNotification dta
                    0x02 -> pure $ Level1Diag dta
                    0x03 -> pure $ SensorData dta
                    0x04 -> pure $ ConfigBlock dta
                    0x05 -> pure $ PreSleepWarning dta
                    0x06 -> pure $ MacroMarkers dta
                    0x07 -> pure $ CollisionDetection dta
                    0x08 -> pure $ OrbPrint dta
                    0x09 -> pure $ OrbErrorASCII dta
                    0x0a -> pure $ OrbErrorBinary dta
                    _    -> fail $ "Unknown async id code 0x"++showHex idCode ""
      where
        getFirst = do
            f <- getWord8
            if f == 0xff
                then getFirst
                else return f

addCK :: Put -> Put
addCK p = do
    let payload = runPut p
    putWord8 0xff
    putWord8 0xff
    putByteString payload
    let ck = sum (B.unpack payload) `xor` 0xff
    putWord8 ck

data SState = SState {
        ssSocket     :: RFCOMMSocket,
        ssNextSeq    :: MVar Word8,
        ssNextThread :: MVar Int,
        ssListeners  :: MVar (Map Int (Chan Reply)),
        ssThread     :: Int
    }

sendCommand :: SState -> Command -> IO Word8
sendCommand ss cmd = do
    seq <- modifyMVar (ssNextSeq ss) $ \seq -> do
        let bs = encode $ Packet seq cmd
        sendAllRFCOMM (ssSocket ss) bs
        return (seq+1, seq)
    return seq

cmd :: Command -> Sphero ByteString
cmd c = Sphero $ do
    ss <- ask
    liftIO $ do
        ch <- newChan
        modifyMVar (ssListeners ss) $ \lnrs -> do
            return (M.insert (ssThread ss) ch lnrs, ())
        do
            seq <- sendCommand ss c
            let await = do
                    r <- readChan ch
                    case r of
                        SimpleResponse seq' body | seq == seq' -> pure body
                        _ -> await
            await
          `finally` do
            modifyMVar (ssListeners ss) $ \lnrs -> do
                return (M.delete (ssThread ss) lnrs, ())

data Versioning = Versioning {
        veRECV  :: Word8,
        veMDL   :: Word8,
        veHW    :: Word8,
        veMSA   :: (Word8, Word8),
        veBL    :: Word8,
        veBAS   :: Word8,
        veMACRO :: Word8,
        veAPI   :: Maybe (Word8, Word8)
    }
    deriving Show

getVersioning :: Sphero Versioning
getVersioning = do
    bs <- cmd GetVersioning
    case runGet go bs of
        Left err -> fail $ "getVersioning failed: "++err
        Right v -> return v
  where
    go = Versioning
        <$> getWord8
        <*> getWord8
        <*> getWord8
        <*> ((,) <$> getWord8 <*> getWord8)
        <*> getWord8
        <*> getWord8
        <*> getWord8
        <*> (do
            r <- remaining
            if r >= 2
                then Just <$> ((,) <$> getWord8 <*> getWord8)
                else pure Nothing
         )

newtype Sphero a = Sphero (ReaderT SState IO a)
    deriving Functor

instance Applicative Sphero where
    pure a = Sphero (pure a)
    Sphero f <*> Sphero a = Sphero (f <*> a)

instance Monad Sphero where
    return a = Sphero (return a)
    Sphero ma >>= kmb = Sphero $ do
        a <- ma
        let Sphero mb = kmb a
        mb

instance MonadIO Sphero where
    liftIO ma = Sphero $ liftIO ma

runSphero :: RFCOMMSocket -> Sphero a -> IO a
runSphero s (Sphero m) = do
    ss <- SState s <$> newMVar 0
                   <*> newMVar 1
                   <*> newMVar M.empty
                   <*> return 0
    t <- forkIO $ do
        let go cont bs0 = do
                bs <- if B.null bs0
                    then recvRFCOMM s 1024
                    else return bs0
                case cont bs of
                    Fail err _   -> fail $ "parse failed: "++err
                    Partial cont -> go cont B.empty
                    Done r bs    -> do
                        chans <- M.elems <$> readMVar (ssListeners ss)
                        forM_ chans $ \ch -> writeChan ch r
                        go (runGetPartial get) bs
        go (runGetPartial get) B.empty
    ret <- runReaderT m ss
    killThread t
    return ret

forkSphero :: Sphero () -> Sphero ThreadId
forkSphero (Sphero m) = Sphero $ do
    ss <- ask
    liftIO $ do
        thr <- modifyMVar (ssNextThread ss) $ \thr -> return (thr+1, thr)
        forkIO $ runReaderT m $ ss { ssThread = thr }

ping :: Sphero ()
ping = do
    cmd Ping
    return ()

color :: Word8  -- ^ Red
      -> Word8  -- ^ Green
      -> Word8  -- ^ Blue
      -> Sphero ()
color r g b = do
    cmd $ Color r g b False
    return ()

-- | Set the Sphero's colour and save it as the default.
colorSave :: Word8  -- ^ Red
          -> Word8  -- ^ Green
          -> Word8  -- ^ Blue
          -> Sphero ()
colorSave r g b = do
    cmd $ Color r g b True
    return ()

roll :: Word8  -- ^ Speed
     -> Int    -- ^ Heading
     -> Sphero ()
roll sp hding = do
    cmd $ Roll sp hding
    return ()
