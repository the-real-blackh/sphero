{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveFunctor #-}
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
import Text.Hexdump


data Command = Ping
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

main :: IO ()
main = do
    Just adapter <- defaultAdapter
    let dev = Device adapter (read "00:06:66:4F:6F:DC")
    s <- openRFCOMM dev 1
    runSphero s $ do
        forkSphero $ forever $ do
            forever $ do
                forM_ [0,5..359] $ \h -> do
                    let (r, g, b) = hsv_to_rgb (h :: Double) 1 1
                    cmd $ Color (toW r) (toW g) (toW b) False
                    liftIO $ threadDelay 20000
        let sp = 40
        forever $ do
            cmd $ Roll sp 0
            liftIO $ threadDelay 2000000
            cmd $ Roll sp 90
            liftIO $ threadDelay 2000000
            cmd $ Roll sp 180
            liftIO $ threadDelay 2000000
            cmd $ Roll sp 270
            liftIO $ threadDelay 2000000
  where
    toW f = max 0 $ min 255 $ floor (f * 255)
