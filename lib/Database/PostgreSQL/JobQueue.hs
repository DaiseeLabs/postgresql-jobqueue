
--
-- Copyright © 2018 Daisee Pty Ltd - All Rights Reserved
--
-- This module was originally created by Christian Marie <christian@daisee.com>
--

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NumDecimals                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | This module provides access to a distributed FIFO queue which can hand out
-- work concurrently with the minimal possible duplication of work (on job
-- failure only).
--
-- For simplicity, we do not store any information on the job other than a JobID
-- UUID, selected by the user on push with 'queue'. This is expected to
-- reference the work stored elsewhere that a worker will subsequently fetch.
--
-- TODO: Track job completion times and worker IDs
module Database.PostgreSQL.JobQueue
(
  withDequeued
, enqueue
, waitForWork
) where

import           Control.Concurrent                 (threadDelay)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8              as C8
import           Data.Monoid
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Options.Applicative                (Parser, auto, help, long,
                                                     metavar, option, short,
                                                     showDefault, value)
import           System.Log.Heavy.LoggingT          (LoggingT)

newtype JobType = JobType UUID

deriving instance Eq   (JobType a)
deriving instance Show (JobType a)

instance ToField (JobType a) where
  toField = Escape . C8.pack . show

-- | Place a job on the queue, FIFO. If successful, a later call to withDequeue
-- will eventually return this same 'JobID'
enqueue :: JobType a -> Connection -> UUID -> IO ()
enqueue jobType conn jid =
    execute conn "INSERT INTO jobs (id, type, retries) VALUES (?, ?, ?)" (pkUuid jid, jobType, 0 :: Int) >>= check1
  where
    check1 1 = return ()
    check1 n = error $ "Expected one row to be inserted in queue, but saw: " <> show n

-- | Get the oldest outstanding piece of work from the queue.
--
-- Upon any exception the work will be re-queued such that the next piece of
-- work requested is different (unless there is only one piece of work). The
-- exception will then be re-thrown.
--
-- This function returns 'Just' the result of the passed continuation on
-- successful completion of work, or 'Nothing' when no work is found or when
-- losing a race with another worker.
withDequeued :: Int
             -- ^ Maximum number of retries. 
             -> UUID
             -- ^ 'UUID' PK
             ^-> Connection 
             -- ^ PG 'Connection'
             -- -> IO (Maybe b)
withDequeued maxRetries jobtype conn k = do
      -- Query inspired by the following:
      -- https://blog.2ndquadrant.com/what-is-select-skip-locked-for-in-postgresql-9-5/
      -- https://metacpan.org/source/SRI/Minion-5.03/lib/Minion/Backend/Pg.pm#L191

      -- First retrieve a job and increment its retries. In the worst case, a
      -- failure now increments a retry without a real attempt at work. That
      -- seems impossible to solve anyway.
      --
      -- We increment the retries before attempting any work to ensure a "fair"
      -- queue that doesn't get stuck on a perpetually failing job at the front of
      -- the queue. We will only retry up to maxRetries times.
      --
      -- The row lock in the first query is not needed for correctness, but
      -- reduces contention and retries.
      inc_r <- query conn
        " UPDATE jobs SET retries = retries + 1 WHERE id = ( \
        \  SELECT id FROM jobs \
        \  WHERE type = ? and retries < ? \
        \  ORDER BY retries, seq_no \
        \  LIMIT 1 \
        \  FOR UPDATE SKIP LOCKED \
        \) RETURNING id;" (jobtype, maxRetries)
      case inc_r of
        [Only jid] ->
          -- We could race with another dequeue, but tryWork will get a row
          -- lock.
          tryWork run jid
        [] ->
          -- No work
          pure Nothing
        xs ->
          -- Should be impossible
          error $ "JobID LIMIT didn't work, got: " <> show (length xs)
    where
      tryWork run jid = withTransaction conn $ do
        r <- query conn
          " DELETE FROM jobs WHERE id = ( \
          \  SELECT id FROM jobs \
          \  WHERE id = ? \
          \  ORDER BY retries \
          \  FOR UPDATE \
          \  SKIP LOCKED \
          \) RETURNING id;" (Only jid)
        case r of
          [Only jid'] -> assert (jid == jid') $
            -- Do the work within the transaction. Any exceptions will be
            -- re-thrown after a roll back. Unexpected termination will
            -- eventually kill the connection, thus transaction, thus re-queue
            -- the work.
            Just <$> run (k (PK jid))
            -- Implicit commit if we got this far
          [] ->
            -- We raced with another dequeue operation and the row was locked
            pure Nothing
          _ ->
            -- Should be impossible
            error $ "Multiple jobs with id: " <> show jid

waitTime :: Int
waitTime = 1e6

-- | Retry if there's no work until there's work, waiting a constant second
-- between retries. Only retry up to maxRetries times.
waitForWork :: JobQueueMaxRetries -> JobType a -> Connection -> (UUID -> LoggingT IO b) -> LoggingT IO b
waitForWork maxRetries jobtype conn k = do
  attempt <- withDequeued maxRetries jobtype conn k
  case attempt of
    Nothing -> do
      liftIO $ threadDelay waitTime
      waitForWork maxRetries jobtype conn k
    Just a -> pure a
