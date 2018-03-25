import           Control.Concurrent
import           Control.Exception          (evaluate)
import           Control.Monad.State.Strict
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Network.Abstract.Lib
import           Network.Abstract.Types
import           System.Random
import           Test.Hspec

getRandomInt :: Int -> Int -> IO Int
getRandomInt mi ma = randomRIO (mi, ma)

getRandomElement :: [a] -> IO a
getRandomElement l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

getRandomAddresses :: IO [NetAddr]
getRandomAddresses = do
  ports <- mapM (\_ -> getRandomInt 2000 7000) [0..10]
  let addrs = map (\p -> ("127.0.0.1", p)) ports
      paddrs = mapM simpleAddrToNetAddr addrs
  case paddrs of
    Left err -> error err
    Right x  -> return x

main :: IO ()
main = spec

spec :: IO ()
spec = hspec $ do
  describe "createUDPNode" $ do
    it "creates nodes which can communicate on UDP with correct addresses" $ do
      addrs <- getRandomAddresses
      let handler addr (sender, bytes) = return ()
      uncs <- mapM (\addr -> createUDPNode addr (handler addr)) addrs
      let specUnc = zip addrs uncs
          boolList = (\(addr, unc) -> addr == selfAddr unc) `map` specUnc
      threadDelay 100000
      and boolList `shouldBe` True

    it "creates nodes whose communication works" $ do
      addrs <- getRandomAddresses
      let handler addr res (sender, bytes) =
            modifyMVar_ res $ \old -> return $ old ++ [(sender, C.unpack bytes)]

      specRes <- mapM (\addr -> do
                          res <- newMVar []
                          return (addr, res)
                      ) addrs
      uncs <- mapM (\(addr, res) -> do
                       node <- createUDPNode addr (handler addr res)
                       return (res, node)
                   ) specRes

      (_, sender) <- getRandomElement uncs
      (res, receiver) <- getRandomElement uncs
      let sAddr = selfAddr sender
          rAddr = selfAddr receiver
          msg   = "test message"

      threadDelay 200000
      sendMsg sender rAddr (C.pack msg)
      threadDelay 200000

      response <- readMVar res
      length response `shouldBe` 1
      fst (head response) `shouldBe` sAddr
      snd (head response) `shouldBe` msg

  describe "createTCPNode" $ do
    it "creates nodes which can communicate on TCP with correct addresses" $ do
      addrs <- getRandomAddresses
      let handler addr (sender, bytes) = return B.empty
      uncs <- mapM (\addr -> createTCPNode addr (handler addr)) addrs
      let specUnc = zip addrs uncs
          boolList = (\(addr, unc) -> addr == selfAddr unc) `map` specUnc
      threadDelay 100000
      and boolList `shouldBe` True

    it "creates nodes whose communication works" $ do
      addrs <- getRandomAddresses
      let handler addr res (sender, bytes) = do
            modifyMVar_ res $ \old -> return $ old ++ [(sender, C.unpack bytes)]
            return B.empty

      specRes <- mapM (\addr -> do
                          res <- newMVar []
                          return (addr, res)
                      ) addrs
      uncs <- mapM (\(addr, res) -> do
                       node <- createTCPNode addr (handler addr res)
                       return (res, node)
                   ) specRes

      (_, sender) <- getRandomElement uncs
      (res, receiver) <- getRandomElement uncs
      let sAddr = selfAddr sender
          rAddr = selfAddr receiver
          msg   = "test message"

      threadDelay 200000
      sendMsg sender rAddr (C.pack msg)
      threadDelay 200000

      response <- readMVar res
      length response `shouldBe` 1
      fst (head response) `shouldBe` sAddr
      snd (head response) `shouldBe` msg

    it "allows replies to function between nodes" $ do
      addrs <- getRandomAddresses
      let handler addr (sender, bytes) = return $ C.pack "reply test"

      uncs <- mapM (\addr -> createTCPNode addr (handler addr)) addrs

      sender <- getRandomElement uncs
      receiver <- getRandomElement uncs
      let sAddr = selfAddr sender
          rAddr = selfAddr receiver

      threadDelay 200000
      rep <- getReply sender rAddr (C.pack "test message")
      threadDelay 200000

      rep `shouldBe` C.pack "reply test"

      threadDelay 200000
      rep2 <- getReply sender rAddr (C.pack "test message 2")
      threadDelay 200000

      rep2 `shouldBe` C.pack "reply test"
