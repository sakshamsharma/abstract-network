import           Control.Exception             (evaluate)
import           System.Random
import           Test.Hspec

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8         as C

import           Network.Abstract.Lib
import           Network.Abstract.Types

getRandomInt :: Int -> Int -> IO Int
getRandomInt mi ma = randomRIO (mi, ma)

getRandomElement :: [a] -> IO a
getRandomElement l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

main :: IO ()
main = do
  ports <- mapM (\_ -> getRandomInt 2000 7000) [0..10]
  let addrs = map (\p -> ("localhost", p)) ports
  spec addrs

spec :: [NetAddr] -> IO ()
spec addrs = hspec $ do
  describe "createFakeNetwork" $ do
    it "creates a fake network with the correct addresses" $ do
      uncs <- createFakeNetwork addrs
      let specUnc = zip addrs uncs
          boolList = (\(addr, unc) -> addr == (selfAddr unc)) `map` specUnc
      and boolList `shouldBe` True

    describe "the created Fake Network" $ do
      it "should be able to send and receive messages" $ do
        uncs <- createFakeNetwork addrs
        sender <- getRandomElement uncs
        receiver <- getRandomElement uncs
        let sAddr = selfAddr sender
            rAddr = selfAddr receiver
            msg   = C.pack "test message"
        (sendMsg sender) rAddr msg
        receivedMsg <- readChan (msgQueue receiver)
        (fst receivedMsg) `shouldBe` sAddr
        (snd receivedMsg) `shouldBe` msg
