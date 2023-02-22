{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Logger
import Database.Persist.Sqlite
import Control.Monad.Reader
import Database.Persist.TH
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Int
  deriving Show
|]

data Env = Env { envConn :: SqlBackend }
newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runAppM :: MonadIO m => Env -> AppM a -> m a
runAppM env (AppM x) = liftIO $ runReaderT x env

runDB :: ReaderT SqlBackend IO a -> AppM a
runDB body = do
  conn <- asks envConn
  liftIO $ runSqlConn body conn

weeklyBudget :: Int
weeklyBudget = 100

spend :: Int -> LineItem -> Int
spend n e = n - (lineItemAmount e)
-- spend n (LineItem { .. }) = n - amount


appMain :: AppM ()
appMain = do
  lineItems <- runDB $ do
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    selectList [] []
  let remainingBudget = foldl' spend weeklyBudget (fmap entityVal lineItems)
  liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget
  
main :: IO ()
main = do 
  runStderrLoggingT $
    withSqliteConn ":memory:" $ \conn -> do 
      runAppM (Env conn) (runDB $ runMigration migrateAll)
      runAppM (Env conn) appMain 