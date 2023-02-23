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
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Logger
import Database.Persist.Sqlite
import Control.Monad.Reader
import Database.Persist.TH
import Data.Maybe
import Database.Esqueleto.Experimental
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Int
  dayPurchased Day
  deriving Show
Budget 
  budget Int
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

getWeeklyBudget :: (MonadIO m) => SqlPersistT m (Maybe (Entity Budget))
getWeeklyBudget = do 
  budget :: Maybe (Entity Budget) <- selectFirst [] []
  pure budget

addBudget :: (MonadIO m) => Int -> SqlPersistT m BudgetId
addBudget x = do 
  budgetId <- insert $ Budget x
  pure budgetId

setBudget :: (MonadIO m) => Int -> BudgetId -> SqlPersistT m BudgetId
setBudget x budgetId = do 
  Database.Persist.Sqlite.update budgetId [BudgetBudget Database.Persist.Sqlite.=. x]
  pure budgetId

setOrAddWeeklyBudget :: (MonadIO m) => Int -> SqlPersistT m BudgetId
setOrAddWeeklyBudget x = do 
  currentBudget <- getWeeklyBudget 
  case currentBudget of 
    Nothing -> addBudget x
    (Just (Entity budgetId _)) -> setBudget x budgetId

getLineItemTotal :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
getLineItemTotal = selectSum $ do
  items <- from $ table @LineItem
  pure $ sum_ $ items ^. LineItemAmount
 where
  selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne

extractBudget :: Maybe (Entity Budget) -> Int 
extractBudget Nothing = 0
extractBudget (Just (Entity _ (Budget amount))) = amount

addItem :: (MonadIO m) => String -> Int -> SqlPersistT m LineItemId
addItem item cost = do 
  today <- liftIO $ utctDay <$> getCurrentTime
  lineItemId <- insert $ LineItem item cost today
  pure lineItemId

appMain :: AppM ()
appMain = do
  total <- runDB $ do
    liftIO $ putStrLn "What is your weekly budget: "
    budget <- liftIO $ getLine 
    liftIO $ putStrLn $ "Weekly budget set to: " ++ budget

    _ <- setOrAddWeeklyBudget (read budget :: Int)

    getLineItemTotal

  budget <- runDB $ getWeeklyBudget

  let remainingBudget = (extractBudget budget) - total
  liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget
  forever $ do
    runDB $ runText
  liftIO $ putStrLn "End"

handleGetUserItem :: (MonadIO m) => SqlPersistT m LineItemId
handleGetUserItem = do 
  liftIO $ putStrLn "What is your item name: "
  name <- liftIO $ getLine 
  liftIO $ putStrLn "What is its cost (in dollars): "
  cost <- liftIO $ getLine
  let costInt = (read cost :: Int)
  liftIO $ putStrLn $ "Adding " ++ name ++ " to your items at $" ++ cost
  addItem name costInt

handleChangeBudget :: (MonadIO m) => SqlPersistT m BudgetId
handleChangeBudget = do 
  liftIO $ putStrLn "What is your new budget: "
  budget <- liftIO $ getLine
  let budgetInt = (read budget :: Int)
  liftIO $ putStrLn $ "Setting budget to $" ++ budget
  setOrAddWeeklyBudget budgetInt

runText :: (MonadIO m) => SqlPersistT m ()
runText = do 
  currentBudget <- getWeeklyBudget
  maybeRemainingBudget :: Maybe (Entity Budget) <- selectFirst [] []
  let budget = extractBudget currentBudget
  spent :: Int <- getLineItemTotal
  liftIO $ putStrLn $ "Your current budget is $(" ++ (show (budget - spent)) ++ "/" ++ (show budget) ++ ")" ++ 
    "\nWould you like to:\n1. Add an item\n2. Change your budget"
  selection <- liftIO $ getLine

  case selection of 
    "1" -> void $ handleGetUserItem
    "2" -> void $ handleChangeBudget

main :: IO ()
main = do 
  runStderrLoggingT $
    withSqliteConn ":memory:" $ \conn -> do 
      runAppM (Env conn) $ do 
        runDB $ runMigration migrateAll
        appMain