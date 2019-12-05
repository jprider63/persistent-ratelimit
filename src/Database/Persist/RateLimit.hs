{-# LANGUAGE FunctionalDependencies, GADTs, FlexibleContexts #-}
module Database.Persist.RateLimit where

import Data.Time.Clock
import Prelude
import Yesod

class RateLimit action entity | action -> entity where
    -- | Number of actions allowed in the defined period. Period is easured in seconds. 
    rateLimit :: action -> (Int, Int)

    -- | Convert a given action and time into an entity. 
    convertAction :: action -> UTCTime -> entity

    -- | Return the field for the time constructor in the entity. 
    timeConstructor :: action -> EntityField entity UTCTime

    -- | Filter to delete the records of a specific action. 
    -- The default is `[]`. 
    -- You probably only need this if multiple rate limiters use the same database entity. 
    deleteFilters :: action -> [Filter entity]
    deleteFilters _ = []

    -- | Filters to specify an action for database queries. 
    rateLimitFilters :: action -> [Filter entity]

-- | Returns the number of actions remaining for the current period. 
numberOfRemainingActions :: (RateLimit action entity, 
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site,
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO Int
numberOfRemainingActions action = do
    let ( limit, period) = rateLimit action
    let timeConstr = timeConstructor action
    let filters = rateLimitFilters action
    now <- liftIO getCurrentTime
    let timeBound = addUTCTime (fromIntegral $ negate period) now
    c <- runDB $  count $ (timeConstr >. timeBound):filters
    return $ limit - c

-- | Determines whether an actions can be performed. 
canPerformAction :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site,
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO Bool
canPerformAction action = 
    fmap (> 0) $ numberOfRemainingActions action

-- | Record when an action occurs. 
recordAction :: (RateLimit action entity,
        BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO ()
recordAction action = do
    now <- liftIO getCurrentTime
    let entity = convertAction action now
    runDB $ insert_ entity

-- | Delete the recorded logs of an action. 
deleteRecordedAction :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site,
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO ()
deleteRecordedAction action =
    let filters = rateLimitFilters action in
    runDB $ deleteWhere filters

-- | Periodically call this function to delete old actions that are past the rate limit period. 
-- A nonsensical action can be used as only the rate limit period will be used. 
cleanOldActions :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        BaseBackend (YesodPersistBackend site) ~ YesodPersistBackend site,
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO ()
cleanOldActions action = do
    let ( _, period) = rateLimit action
    let timeConstr = timeConstructor action
    let filters = deleteFilters action
    now <- liftIO getCurrentTime
    let timeBound = addUTCTime (fromIntegral $ negate period) now
    runDB $ deleteWhere $ (timeConstr <. timeBound):filters
