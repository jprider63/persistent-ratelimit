{-# LANGUAGE FunctionalDependencies, GADTs, FlexibleContexts #-}
module Database.Persistent.RateLimit where

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

    -- | Optionally add additional filters for database queries. 
    -- The default is `[]`. 
    -- You probably only need this if multiple rate limiters use the same entity. 
    rateLimitFilters :: action -> [Filter entity]
    rateLimitFilters _ = []

-- | Returns the number of actions remaining for the current period. 
numberOfRemainingActions :: (RateLimit action entity, 
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO Int
numberOfRemainingActions action = do
    let ( limit, period) = rateLimit action
    let timeConstr = timeConstructor action
    let filters = rateLimitFilters action
    now <- lift getCurrentTime
    let timeBound = addUTCTime (fromIntegral $ negate period) now
    c <- runDB $  count $ (timeConstr >. timeBound):filters
    return $ limit - c

-- | Determines whether an actions can be performed. 
canPerformAction :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO Bool
canPerformAction action = 
    numberOfRemainingActions action >>= return . (> 0)

-- | Record when an action occurs. 
recordAction :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO ()
recordAction action = do
    now <- lift getCurrentTime
    let entity = convertAction action now
    runDB $ insert_ entity

-- | Periodically call this function to delete old actions that are past the rate limit period. 
-- A nonsensical action can be used as only the rate limit period will be used. 
cleanOldActions :: (RateLimit action entity,
        PersistEntityBackend entity ~ YesodPersistBackend site, 
        YesodPersist site, 
        PersistEntity entity, 
        PersistQuery (YesodPersistBackend site)) => 
    action -> HandlerT site IO ()
cleanOldActions action = do
    let ( _, period) = rateLimit action
    let timeConstr = timeConstructor action
    let filters = rateLimitFilters action
    now <- lift getCurrentTime
    let timeBound = addUTCTime (fromIntegral $ negate period) now
    runDB $ deleteWhere $ (timeConstr <=. timeBound):filters
