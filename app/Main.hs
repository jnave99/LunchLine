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
import System.Exit (exitSuccess)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
LineItem
  name String
  amount Double
  dayPurchased Day
  deriving Show
Budget 
  budget Double
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

addBudget :: (MonadIO m) => Double -> SqlPersistT m BudgetId
addBudget x = do 
  budgetId <- insert $ Budget x
  pure budgetId

setBudget :: (MonadIO m) => Double -> BudgetId -> SqlPersistT m BudgetId
setBudget x budgetId = do 
  Database.Persist.Sqlite.update budgetId [BudgetBudget Database.Persist.Sqlite.=. x]
  pure budgetId

setOrAddWeeklyBudget :: (MonadIO m) => Double -> SqlPersistT m BudgetId
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

extractBudget :: Maybe (Entity Budget) -> Double 
extractBudget Nothing = 0
extractBudget (Just (Entity _ (Budget amount))) = amount

addItem :: (MonadIO m) => String -> Double -> SqlPersistT m LineItemId
addItem item cost = do 
  today <- liftIO $ utctDay <$> getCurrentTime
  lineItemId <- insert $ LineItem item cost today
  pure lineItemId

getDate :: (MonadIO m) => Bool -> SqlPersistT m Day
getDate isAsc = do 
  let today = liftIO $ utctDay <$> getCurrentTime

  item :: Maybe (Entity LineItem) <- do 
    case isAsc of 
      True -> selectFirst [] [Asc LineItemDayPurchased]
      False -> selectFirst [] [Desc LineItemDayPurchased]

  case item of 
    Nothing -> today
    (Just (Entity _ (LineItem _ _ day))) -> pure day

notZero :: (Num a, Eq a) => a -> a 
notZero a 
  | a == 0 = 1
  | otherwise = a

getDailyAverageSpend :: (MonadIO m) => SqlPersistT m Double
getDailyAverageSpend = do 
  totalSpend <- getLineItemTotal
  firstDay <- getDate True 
  lastDay <- getDate False

  pure $ totalSpend / notZero (fromIntegral (diffDays firstDay lastDay))

appMain :: AppM ()
appMain = do
  total <- runDB $ do
    liftIO $ putStrLn "What is your weekly budget: "
    budget <- liftIO $ getLine 
    liftIO $ putStrLn $ "Weekly budget set to: " ++ budget

    _ <- setOrAddWeeklyBudget (read budget :: Double)

    getLineItemTotal

  budget <- runDB $ getWeeklyBudget

  let remainingBudget = (extractBudget budget) - total
  liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget
  _ <- forever $ do
    runDB $ runText
  liftIO $ putStrLn "End"

handleGetUserItem :: (MonadIO m) => SqlPersistT m LineItemId
handleGetUserItem = do 
  liftIO $ putStrLn "What is your item name: "
  name <- liftIO $ getLine 
  liftIO $ putStrLn "What is its cost (in dollars): "
  cost <- liftIO $ getLine
  let costDouble = (read cost :: Double)
  liftIO $ putStrLn $ "Adding " ++ name ++ " to your items at $" ++ cost
  addItem name costDouble

handleChangeBudget :: (MonadIO m) => SqlPersistT m BudgetId
handleChangeBudget = do 
  liftIO $ putStrLn "What is your new budget: "
  budget <- liftIO $ getLine
  let budgetDouble = (read budget :: Double)
  liftIO $ putStrLn $ "Setting budget to $" ++ budget
  setOrAddWeeklyBudget budgetDouble

runText :: (MonadIO m) => SqlPersistT m ()
runText = do 
  currentBudget <- getWeeklyBudget
  dailySpend <- getDailyAverageSpend
  let budget = extractBudget currentBudget
  spent :: Double <- getLineItemTotal
  liftIO $ putStrLn $ "Your current budget is $(" ++ (show (budget - spent)) ++ "/" ++ (show budget) ++ ")" ++ 
    "\nWould you like to:\n1. Add an item\n2. Change your budget\n3. See statistics\n4. Exit"
  selection <- liftIO $ getLine

  case selection of 
    "1" -> void $ handleGetUserItem
    "2" -> void $ handleChangeBudget
    "3" -> liftIO $ putStrLn (show dailySpend)
    "4" -> liftIO $ exitSuccess
    _ -> liftIO $ putStrLn "Error: Not an option"

main :: IO ()
main = do 
  runStderrLoggingT $
    withSqliteConn ":memory:" $ \conn -> do 
      runAppM (Env conn) $ do 
        runDB $ runMigration migrateAll
        appMain