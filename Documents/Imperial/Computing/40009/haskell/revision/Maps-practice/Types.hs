module Types where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

-- A User is represented by their unique username
type User = String

-- A directed edge representing a "follows" relationship
-- (follower, followee) means "follower follows followee"
type Follow = (User, User)

-- A social network is represented as a Map from users to the set of users they follow
type Network = Map User (Set User)

-- A community is a set of users who are densely connected
type Community = Set User

-- Engagement metrics for a user
data Metrics = Metrics
  { followers   :: Int      -- Number of followers
  , following   :: Int      -- Number of users this user follows
  , mutuals     :: Int      -- Number of mutual connections
  , influence   :: Double   -- Influence score (followers / following ratio)
  } deriving (Eq, Show)

-- A path between two users (list of users from source to target)
type Path = [User]

-- Recommendation with score
type Recommendation = (User, Int)

---------------------------------------------------------
-- Pretty printing helpers

showNetwork :: Network -> IO ()
showNetwork net = mapM_ showUser (M.toList net)
  where
    showUser (u, fs) = putStrLn $ u ++ " follows: " ++ show (S.toList fs)

showMetrics :: User -> Metrics -> IO ()
showMetrics u m = do
  putStrLn $ "=== Metrics for " ++ u ++ " ==="
  putStrLn $ "  Followers:  " ++ show (followers m)
  putStrLn $ "  Following:  " ++ show (following m)
  putStrLn $ "  Mutuals:    " ++ show (mutuals m)
  putStrLn $ "  Influence:  " ++ show (influence m)

showCommunities :: [Community] -> IO ()
showCommunities cs = mapM_ showOne (zip [1..] cs)
  where
    showOne (i, c) = putStrLn $ "Community " ++ show i ++ ": " ++ show (S.toList c)

showRecommendations :: [Recommendation] -> IO ()
showRecommendations rs = mapM_ showRec rs
  where
    showRec (u, score) = putStrLn $ "  " ++ u ++ " (score: " ++ show score ++ ")"
