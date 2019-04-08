--
-- Copyright © 2018 Daisee Pty Ltd - All Rights Reserved
--

{-# LANGUAGE OverloadedStrings #-}

module JobQueueSpec where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception            (bracket)
import           Control.Monad                (replicateM, replicateM_)
import           Control.Monad.IO.Class
import           Data.Foldable                (for_)
import           Data.List                    (sort)
import           Data.Traversable             (for)
import           Data.UUID.V4                 (nextRandom)
import           Database.PostgreSQL.JobQueue
import           Database.PostgreSQL.Simple
import           Test.Hspec

n :: Int
n = 10 -- number of threads to test concurrency with

withDB :: (Connection -> IO ()) -> IO ()
withDB = bracket connectTestDB close

nextRandomPK :: IO UUID
nextRandomPK = PK <$> nextRandom

spec :: Spec
spec = do
    around withDB $ describe "job queue one thread" $ do
      let mxRetr = JobQueueMaxRetries 3
      it "begins with no work" $ \conn ->
          Log.discard (withDequeued mxRetr Ceres conn pure) >>=
            liftIO . (`shouldBe` Nothing)

      it "queues then dequeues" $ \conn -> do
          jid <- nextRandomPK
          enqueue Ceres conn jid
          Log.discard $ do
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Just jid)
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Nothing)

      it "ignores different types" $ \_conn -> do
          conn <- connectTestDB
          jid <- nextRandomPK
          enqueue Transcribe conn jid

          Log.discard $ do
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Nothing)
            withDequeued mxRetr Transcribe conn pure >>=
              liftIO . (`shouldBe` Just jid)

      -- We need to make progress under failure
      it "puts back a failing job and does not recieve the failed one immediately" $ \conn -> do
          (jid1, jid2) <- (,) <$> nextRandomPK <*> nextRandomPK
          enqueue Ceres conn jid1
          enqueue Ceres conn jid2

          Log.discard (withDequeued mxRetr Ceres conn (\_jid -> error "rollback please")) `shouldThrow` anyErrorCall
          threadDelay 1000000
          Log.discard $ do
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Just jid2)
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Just jid1)

      it "is FIFO" $ \_conn -> do
          conn <- connectTestDB
          jid1 <- nextRandomPK
          jid2 <- nextRandomPK
          enqueue Ceres conn jid1
          enqueue Ceres conn jid2
          Log.discard $ do
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Just jid1)
            withDequeued mxRetr Ceres conn pure >>=
              liftIO . (`shouldBe` Just jid2)

      it "retries up to maxRetries times" $ \conn -> do
        jid <- nextRandomPK
        enqueue Ceres conn jid
        let (JobQueueMaxRetries imxRetr) = mxRetr
        replicateM_ imxRetr $
          Log.discard (withDequeued mxRetr Ceres conn (\_ -> error "put it back on job queue")) `shouldThrow` anyErrorCall

        Log.discard $ do
          withDequeued mxRetr Ceres conn pure >>= liftIO . (`shouldBe` Nothing)

    describe "job queue multiple threads" $
      -- makes sure that a lock is not hold by a single thread so that other"
      -- threads need to wait for that lock to be released, eg it should "
      -- be real concurrency
      it "permits multiple concurrent workers" $ do
          let mxRetr = 3
          let nTimes = replicateM n
          succ_conns <- nTimes connectTestDB
          _err_conns <- nTimes connectTestDB
          jids <- nTimes nextRandomPK

          -- Create a bunch of jobs
          creating <- for (zip succ_conns jids) $ \(conn, jid) ->
              async $ enqueue Ceres conn jid
          for_ creating wait

          -- Start consuming a bunch of jobs.
          -- TODO: Check that time elapsed is < threadDelay + buffer, so that
          -- we know if sequencing is happening and thus have a time elapsed
          -- closer to n * threadDelay
          --
          -- Once this is done we can lower threadDelay, as it's human-sized
          -- now.
          threads <- for succ_conns $ \conn ->
              -- Sleep for one second to ensure that we're not handing out
              -- jobs in sequence, but in parallel.
              async $ Log.discard $ waitForWork mxRetr Ceres conn (\x -> liftIO $ threadDelay 1000000 >> return x)

          dequeued <- for threads wait
          sort dequeued `shouldBe` sort jids
