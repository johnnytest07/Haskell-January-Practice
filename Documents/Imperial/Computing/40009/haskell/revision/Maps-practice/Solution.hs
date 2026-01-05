module Solution where

import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

import Types
import Examples

---------------------------------------------------------
-- Helper function (given)

-- Safely look up a user's following set, returning empty set if not found
getFollowing :: User -> Network -> Set User
getFollowing u net = M.findWithDefault S.empty u net

---------------------------------------------------------
-- Part I

-- 1. Build a network from a list of follow relationships
-- Each Follow (a, b) means user 'a' follows user 'b'
-- Both users should appear as keys in the resulting map
buildNetwork :: [Follow] -> Network
buildNetwork fs = foldr addFollow M.empty fs
  where
    addFollow (a, b) net = M.insertWith S.union a (S.singleton b) 
                         $ M.insertWith S.union b S.empty net

-- 2. Get all users in the network (both followers and followees)
allUsers :: Network -> Set User
allUsers net = S.union (M.keysSet net) (S.unions (M.elems net))

-- 3. Get the set of followers for a given user
getFollowers :: User -> Network -> Set User
getFollowers u net = M.keysSet $ M.filter (S.member u) net

-- 4. Check if two users mutually follow each other
areMutual :: User -> User -> Network -> Bool
areMutual u1 u2 net = S.member u2 (getFollowing u1 net) 
                   && S.member u1 (getFollowing u2 net)

-- 5. Get all mutual connections for a user
getMutuals :: User -> Network -> Set User
getMutuals u net = S.intersection (getFollowing u net) (getFollowers u net)

---------------------------------------------------------
-- Part II

-- 1. Calculate engagement metrics for a user
calculateMetrics :: User -> Network -> Metrics
calculateMetrics u net = Metrics numFollowers numFollowing numMutuals inf
  where
    numFollowers = S.size $ getFollowers u net
    numFollowing = S.size $ getFollowing u net
    numMutuals   = S.size $ getMutuals u net
    inf          = if numFollowing == 0 
                   then 0.0 
                   else fromIntegral numFollowers / fromIntegral numFollowing

-- 2. Find users with most followers (top influencers)
-- Return the top n users sorted by follower count (descending)
topInfluencers :: Int -> Network -> [User]
topInfluencers n net = take n $ sortOn key $ S.toList (allUsers net)
  where
    key u = (negate $ S.size $ getFollowers u net, u)

-- 3. Suggest friends based on "friends of friends" algorithm
-- Returns users that are followed by people the user follows,
-- but not already followed by the user (and not the user themselves)
-- Sorted by number of mutual connections (descending)
suggestFriends :: User -> Network -> [Recommendation]
suggestFriends u net = sortOn (negate . snd) recs
  where
    myFollowing  = getFollowing u net
    -- All users followed by people I follow
    friendsOfFriends = S.unions $ S.map (`getFollowing` net) myFollowing
    -- Exclude myself and people I already follow
    candidates   = S.difference friendsOfFriends (S.insert u myFollowing)
    -- For each candidate, count how many of my followings also follow them
    score cand   = S.size $ S.filter (`followsUser` cand) myFollowing
    followsUser f c = S.member c (getFollowing f net)
    recs         = [(c, score c) | c <- S.toList candidates, score c > 0]

---------------------------------------------------------
-- Part III

-- 1. Find if there exists a path from one user to another
-- (following the "follows" direction)
hasPath :: User -> User -> Network -> Bool
hasPath src tgt net
  | src == tgt = True
  | otherwise  = go (S.singleton src) (S.singleton src)
  where
    go visited frontier
      | S.null frontier     = False
      | S.member tgt frontier = True
      | otherwise           = go visited' frontier'
      where
        neighbors = S.unions $ S.map (`getFollowing` net) frontier
        frontier' = S.difference neighbors visited
        visited'  = S.union visited frontier'

-- 2. Find the shortest path between two users (BFS)
-- Returns Nothing if no path exists
shortestPath :: User -> User -> Network -> Maybe Path
shortestPath src tgt net
  | src == tgt = Just [src]
  | otherwise  = bfs (S.singleton src) [[(src)]]
  where
    bfs :: Set User -> [Path] -> Maybe Path
    bfs visited [] = Nothing
    bfs visited paths
      | null paths' = Nothing
      | otherwise   = case find ((== tgt) . head) paths' of
                        Just p  -> Just (reverse p)
                        Nothing -> bfs visited' paths'
      where
        -- Extend each path by one step
        extend path@(u:_) = 
          [(v:path) | v <- S.toList (getFollowing u net), S.notMember v visited]
        paths' = concatMap extend paths
        visited' = S.union visited (S.fromList $ map head paths')

-- 3. Find connected components in the network
-- (treating follows as undirected edges)
findCommunities :: Network -> [Community]
findCommunities net = go (allUsers net) []
  where
    -- Build undirected adjacency: union of following and followers
    getNeighbors u = S.union (getFollowing u net) (getFollowers u net)
    
    go remaining comms
      | S.null remaining = comms
      | otherwise        = go remaining' (component : comms)
      where
        start     = S.findMin remaining
        component = explore (S.singleton start) (S.singleton start)
        remaining'= S.difference remaining component
    
    explore visited frontier
      | S.null frontier = visited
      | otherwise       = explore visited' frontier'
      where
        neighbors = S.unions $ S.map getNeighbors frontier
        frontier' = S.difference neighbors visited
        visited'  = S.union visited frontier'

---------------------------------------------------------
-- Part IV (Extension)

-- 1. Calculate the "degree of separation" between two users
-- Returns Nothing if users are not connected
degreeOfSeparation :: User -> User -> Network -> Maybe Int
degreeOfSeparation u1 u2 net = fmap (\p -> length p - 1) (shortestPath u1 u2 net)

-- 2. Find all users within n degrees of separation from a user
usersWithinDegrees :: Int -> User -> Network -> Set User
usersWithinDegrees n u net = go n (S.singleton u) (S.singleton u)
  where
    go 0 visited _ = visited
    go k visited frontier
      | S.null frontier = visited
      | otherwise       = go (k - 1) visited' frontier'
      where
        neighbors = S.unions $ S.map (`getFollowing` net) frontier
        frontier' = S.difference neighbors visited
        visited'  = S.union visited frontier'

-- 3. Calculate the clustering coefficient for a user
-- This measures how connected a user's friends are to each other
-- Returns the ratio of actual connections between friends 
-- to the maximum possible connections
clusteringCoefficient :: User -> Network -> Double
clusteringCoefficient u net
  | n < 2     = 0.0
  | otherwise = fromIntegral actualEdges / fromIntegral maxEdges
  where
    friends     = getFollowing u net
    n           = S.size friends
    maxEdges    = n * (n - 1)  -- directed graph: n*(n-1) possible edges
    -- Count edges between friends (i follows j where both are in friends)
    actualEdges = sum [S.size (S.intersection (getFollowing f net) friends) 
                      | f <- S.toList friends]
