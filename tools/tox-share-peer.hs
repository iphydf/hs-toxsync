{-# LANGUAGE LambdaCase #-}
-- | A Tox client for testing file sharing against other clients, such as
-- Green-Sky's totato. It can run a local network of DHT nodes with a TCP
-- relay, found a private group and invite its friends, join a group it's
-- invited to, share a file, and download every file shared in the group. It
-- keeps the group's messages, answers HS2 history requests with them, and
-- asks each peer that joins for its history. It prints what happens, a line
-- each, for scripts to wait on.
module Main (main) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forM_, unless, when)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           Data.IORef
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time.Clock.POSIX  (getPOSIXTime)
import           Data.Word              (Word16)
import           GHC.Clock              (getMonotonicTime)
import           System.Environment     (getArgs)
import           System.FilePath        (takeFileName, (</>))
import           System.IO              (BufferMode (..), IOMode (..),
                                         SeekMode (..), hSeek, hSetBuffering,
                                         stdout, withBinaryFile)
import           System.Random          (initStdGen, uniform)

import           Network.Tox.C
import qualified Tox.Share              as Share
import           Tox.Share              (Share)
import           Tox.Share.History      (Body (..), Entry (..), Range (..))
import qualified Tox.Share.Info         as Info
import           Tox.Share.Info         (Info (..))
import qualified Tox.Share.Packet       as Packet
import           Tox.Share.Packet       (Channel (..))

data Config = Config
  { dhtNodes   :: Int
  , relayPort  :: Word16
  , bootstraps :: [String]
  , relays     :: [String]
  , adds       :: [String]
  , groupName  :: Maybe String
  , saveTo     :: Maybe FilePath
  , toShare    :: Maybe FilePath
  , sharePeers :: Int
  , says       :: [String]
  }

config :: [String] -> Config
config = go (Config 0 0 [] [] [] Nothing Nothing Nothing 1 [])
  where
    go c = \case
      "--dht-nodes" : n : rest -> go c { dhtNodes = read n } rest
      "--tcp-relay-port" : n : rest -> go c { relayPort = read n } rest
      "--bootstrap" : n : rest -> go c { bootstraps = bootstraps c <> [n] } rest
      "--tcp-relay" : n : rest -> go c { relays = relays c <> [n] } rest
      "--add" : toxId : rest -> go c { adds = adds c <> [toxId] } rest
      "--group-new" : n : rest -> go c { groupName = Just n } rest
      "--download" : dir : rest -> go c { saveTo = Just dir } rest
      "--share" : path : rest -> go c { toShare = Just path } rest
      "--share-peers" : n : rest -> go c { sharePeers = read n } rest
      "--say" : text : rest -> go c { says = says c <> [text] } rest
      [] -> c
      arg : _ -> error ("unknown argument " <> arg)

-- | The largest custom lossy group packet c-toxcore sends, less the 4 bytes
-- FT1 takes.
maxData :: Int
maxData = 1373 - 4

must :: Show e => IO (Either e a) -> IO a
must = (>>= either (fail . show) pure)

hex :: BS.ByteString -> String
hex = Char8.unpack . Base16.encode

-- | @host:port:key@
node :: String -> (String, Word16, DhtId)
node s = case break (== ':') s of
  (host, ':' : rest) | (port, ':' : key) <- break (== ':') rest, Right bytes <- Base16.decode (Char8.pack key), Just k <- fromBytes bytes -> (host, read port, k)
  _ -> error ("bad node " <> s)

withToxes :: Options -> [Options] -> ([Tox] -> IO a) -> IO a
withToxes _ [] f = f []
withToxes base (o : os) f = must (withTox o (\t -> withToxes base os (f . (t :))))

data State = State
  { share   :: Share GroupPeerNumber
  , group   :: Maybe GroupNumber
  , invited :: [FriendNumber]
  , peers   :: Int
  , shared  :: Bool
  , infos   :: Map.Map BS.ByteString Info
  , sharing :: Map.Map BS.ByteString BS.ByteString
    -- ^ The files we share, whole.
  , said    :: Bool
  , history :: [Entry]
    -- ^ The group's messages, newest first.
  , asking  :: [(Double, GroupPeerNumber)]
    -- ^ Peers to ask for history, and when.
  }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  c <- config <$> getArgs
  defaults <- must defaultOptions
  let base = defaults { ipv6Enabled = False, localDiscoveryEnabled = False }
      nodeOptions = [base { tcpPort = if i == 0 then relayPort c else 0 } | i <- [0 .. dhtNodes c - 1]]
  withToxes base (base : nodeOptions) $ \case
    [] -> fail "no tox"
    tox : nodes -> do
      forM_ (zip nodes [0 :: Int ..]) $ \(n, i) -> do
        key <- toxSelfGetDhtId n
        port <- must (toxSelfGetUdpPort n)
        putStrLn ("DHT_NODE 127.0.0.1:" <> show port <> ":" <> hex (toBytes key))
        when (i == 0 && relayPort c /= 0) $ putStrLn ("TCP_RELAY 127.0.0.1:" <> show (relayPort c) <> ":" <> hex (toBytes key))
        forM_ (take 1 (drop (i - 1) nodes)) $ \prev -> when (i > 0) $ toxSelfGetDhtId prev >>= \k -> toxSelfGetUdpPort prev >>= either (const (pure ())) (\p -> () <$ toxBootstrap n "127.0.0.1" p k)
        _ <- toxBootstrap tox "127.0.0.1" port key
        pure ()
      forM_ (map node (bootstraps c)) $ \(host, port, key) -> must (toxBootstrap tox host port key)
      forM_ (map node (relays c)) $ \(host, port, key) -> must (toxAddTcpRelay tox host port key)
      own <- toxSelfGetDhtId tox
      ownPort <- must (toxSelfGetUdpPort tox)
      putStrLn ("NODE 127.0.0.1:" <> show ownPort <> ":" <> hex (toBytes own))
      toxSelfGetAddress tox >>= putStrLn . ("TOX_ID " <>) . hex . toBytes
      forM_ (adds c) $ \toxId -> case Base16.decode (Char8.pack toxId) of
        Right bytes | Just address <- fromBytes bytes -> must (toxFriendAdd tox address (Char8.pack "share")) >>= print
        _ -> fail ("bad tox id " <> toxId)
      g <- initStdGen
      start <- getMonotonicTime
      state <- newIORef (State (Share.new 0 maxData g) Nothing [] 0 False Map.empty Map.empty False [] [])
      let now = subtract start <$> getMonotonicTime
          apply f = do
            t <- now
            s <- readIORef state
            let (sh, outs) = f t (share s)
            writeIORef state s { share = sh }
            mapM_ (output t) outs
          output _ o = readIORef state >>= \s -> case (o, group s) of
            (Share.Send p packet, Just gr) -> () <$ toxGroupSendCustomPrivatePacket tox gr p (Packet.lossless packet) (Packet.encode packet)
            (Share.SendAll packet, Just gr) -> () <$ toxGroupSendCustomPacket tox gr True (Packet.encode packet)
            (Share.Shared p _ fid, _) -> do
              putStrLn ("SHARED " <> hex fid)
              when (saveTo c /= Nothing) $ modifyIORef' state (\st -> st { share = Share.download p fid (share st) })
            (Share.Learned fid inf, _) -> do
              putStrLn ("LEARNED " <> Char8.unpack (name inf) <> " " <> show (size inf))
              modifyIORef' state (\st -> st { infos = Map.insert fid inf (infos st) })
              forM_ (saveTo c) $ \dir -> withBinaryFile (dir </> fileName inf) WriteMode (const (pure ()))
            (Share.Chunk fid n bytes, _) -> forM_ ((,) <$> saveTo c <*> Map.lookup fid (infos s)) $ \(dir, inf) ->
              withBinaryFile (dir </> fileName inf) ReadWriteMode $ \h -> do
                hSeek h AbsoluteSeek (fromIntegral n * fromIntegral (chunkSize inf))
                BS.hPut h bytes
            (Share.ChunkWanted p fid n, _) -> forM_ (Map.lookup fid (infos s)) $ \inf -> do
              putStrLn ("SERVING " <> show p <> " " <> show n)
              let at = fromIntegral n * fromIntegral (chunkSize inf)
                  len = Info.chunkLength inf n
              bytes <- case (Map.lookup fid (sharing s), saveTo c) of
                (Just whole, _) -> pure (BS.take len (BS.drop at whole))
                (_, Just dir) -> withBinaryFile (dir </> fileName inf) ReadMode $ \h -> hSeek h AbsoluteSeek (fromIntegral at) >> BS.hGet h len
                _ -> pure BS.empty
              apply (\t -> Share.sendChunk t p fid n bytes)
            (Share.HistoryWanted p range, _) -> do
              putStrLn ("HISTORY_WANTED " <> show (newest range) <> " " <> show (oldest range))
              let within e = time e <= 10 * newest range && time e >= 10 * oldest range
              apply (\t -> Share.answerHistory t p range (filter within (history s)))
            (Share.History _ entries, _) -> do
              putStrLn ("HISTORY " <> show (length entries))
              forM_ entries $ \e -> case body e of
                Text _ text  -> putStrLn ("HISTORY_TEXT " <> Text.unpack text)
                Shared _ fid -> putStrLn ("HISTORY_FILE " <> hex fid)
            (Share.Finished fid, _) -> forM_ ((,) <$> saveTo c <*> Map.lookup fid (infos s)) $ \(dir, inf) -> do
              bytes <- BS.readFile (dir </> fileName inf)
              putStrLn ("DOWNLOADED " <> fileName inf <> " " <> show (BS.length bytes) <> " " <> hex (Info.sha1 bytes))
            _ -> pure ()
          remember e = modifyIORef' state (\s -> s { history = e : history s })
          deciseconds = (`div` 100) . floor . (* 1000) <$> getPOSIXTime
          sayIfReady = readIORef state >>= \s -> case group s of
            Just gr | not (said s) && not (null (says c)) && peers s >= 1 -> do
              writeIORef state s { said = True }
              key <- must (toxGroupSelfGetPublicKey tox gr)
              forM_ (says c) $ \text -> do
                GroupMessageId m <- must (toxGroupSendMessage tox gr MessageTypeNormal (Char8.pack text))
                ts <- deciseconds
                remember (Entry ts (toBytes key) m (Text False (Text.pack text)))
                putStrLn ("SAID " <> text)
            _ -> pure ()
          fileName = takeFileName . Char8.unpack . name
          shareIfReady = readIORef state >>= \s -> case (toShare c, group s) of
            (Just path, Just _) | not (shared s) && peers s >= sharePeers c -> do
              bytes <- BS.readFile path
              let chunkLen = Info.chunkSizeFor (fromIntegral (BS.length bytes))
                  pieces = [BS.take (fromIntegral chunkLen) (BS.drop (i * fromIntegral chunkLen) bytes) | i <- [0 .. (BS.length bytes - 1) `div` fromIntegral chunkLen]]
                  inf = Info (Char8.pack (takeFileName path)) (fromIntegral (BS.length bytes)) chunkLen (BS.concat (map Info.sha1 pieces))
                  (m, _) = uniform g
              writeIORef state s { shared = True, infos = Map.insert (Info.hash inf) inf (infos s), sharing = Map.insert (Info.hash inf) bytes (sharing s) }
              putStrLn ("SHARING " <> hex (Info.hash inf))
              apply (\_ -> Share.share m inf)
            _ -> pure ()
      forM_ (groupName c) $ \gname -> do
        gr <- must (toxGroupNew tox GroupPrivacyStatePrivate (Char8.pack gname) (Char8.pack "share-peer"))
        modifyIORef' state (\s -> s { group = Just gr })
        must (toxGroupGetChatId tox gr) >>= putStrLn . ("GROUP_CHAT_ID " <>) . hex . toBytes
      let loop = do
            mapM_ (\n -> () <$ toxEventsIterate n) nodes
            events <- must (toxEventsIterate tox)
            forM_ events $ \case
              SelfConnectionStatus e -> putStrLn ("SELF_CONNECTION " <> show (selfConnectionStatus e))
              FriendRequest e -> must (toxFriendAddNorequest tox (friendRequestPublicKey e)) >>= putStrLn . ("FRIEND_ADDED " <>) . show
              FriendConnectionStatus e -> do
                putStrLn ("FRIEND_CONNECTION " <> show (friendConnectionStatusFriendNumber e) <> " " <> show (friendConnectionStatus e))
                s <- readIORef state
                case group s of
                  Just gr | groupName c /= Nothing, friendConnectionStatus e /= ConnectionNone, friendConnectionStatusFriendNumber e `notElem` invited s -> do
                    r <- toxGroupInviteFriend tox gr (friendConnectionStatusFriendNumber e)
                    putStrLn ("GROUP_INVITED " <> show r)
                    writeIORef state s { invited = friendConnectionStatusFriendNumber e : invited s }
                  _ -> pure ()
              GroupInvite e -> readIORef state >>= \s -> unless (group s /= Nothing) $ do
                gr <- must (toxGroupInviteAccept tox (groupInviteFriendNumber e) (groupInviteInviteData e) (Char8.pack "share-peer") BS.empty)
                writeIORef state s { group = Just gr }
                putStrLn "GROUP_INVITE_ACCEPTED"
              GroupSelfJoin _ -> putStrLn "GROUP_SELF_JOIN"
              GroupPeerJoin e -> do
                modifyIORef' state (\s -> s { peers = peers s + 1 })
                putStrLn ("GROUP_PEER_JOIN " <> show (groupPeerJoinPeerId e))
                -- As totato does, a few seconds after the peer joins.
                t <- now
                modifyIORef' state (\s -> s { asking = asking s <> [(t + 5, groupPeerJoinPeerId e)] })
                apply Share.joined
              GroupMessage e -> do
                key <- must (toxGroupPeerGetPublicKey tox (groupMessageGroupNumber e) (groupMessagePeerId e))
                ts <- deciseconds
                let GroupMessageId m = groupMessageMessageId e
                    text = Text.decodeUtf8With (\_ _ -> Just '?') (groupMessage e)
                remember (Entry ts (toBytes key) m (Text (groupMessageMessageType e == MessageTypeAction) text))
                putStrLn ("MESSAGE " <> Text.unpack text)
              GroupPeerExit e -> do
                putStrLn ("GROUP_PEER_EXIT " <> show (groupPeerExitPeerId e))
                apply (\_ -> Share.leave (groupPeerExitPeerId e))
              GroupCustomPacket e -> apply (\t -> Share.receive t (groupCustomPacketPeerId e) Public (groupCustomPacketData e))
              GroupCustomPrivatePacket e -> apply (\t -> Share.receive t (groupCustomPrivatePacketPeerId e) Private (groupCustomPrivatePacketData e))
              _ -> pure ()
            shareIfReady
            sayIfReady
            t <- now
            (due, later) <- span ((<= t) . fst) . asking <$> readIORef state
            modifyIORef' state (\s -> s { asking = later })
            forM_ due $ \(_, p) -> do
              unixNow <- floor <$> getPOSIXTime
              apply (\_ -> Share.requestHistory unixNow p)
            apply Share.tick
            threadDelay 5000
            loop
      loop
