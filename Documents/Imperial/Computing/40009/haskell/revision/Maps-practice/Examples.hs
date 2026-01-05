module Examples where

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

import Types

---------------------------------------------------------
-- Test Networks

-- Empty network
net0 :: Network
net0 = M.empty

-- Simple network: Alice follows Bob
net1 :: Network
net1 = M.fromList
  [ ("alice", S.singleton "bob")
  , ("bob", S.empty)
  ]

-- Small network with mutual following
net2 :: Network
net2 = M.fromList
  [ ("alice", S.fromList ["bob", "charlie"])
  , ("bob", S.fromList ["alice", "charlie"])
  , ("charlie", S.singleton "alice")
  ]

-- Medium network (social media scenario)
net3 :: Network
net3 = M.fromList
  [ ("alice", S.fromList ["bob", "charlie", "diana"])
  , ("bob", S.fromList ["alice", "charlie"])
  , ("charlie", S.fromList ["alice", "bob", "diana", "eve"])
  , ("diana", S.fromList ["eve"])
  , ("eve", S.fromList ["charlie", "diana"])
  ]

-- Larger network with clear community structure
net4 :: Network
net4 = M.fromList
  [ -- Community 1: Tech enthusiasts
    ("alice", S.fromList ["bob", "charlie", "diana"])
  , ("bob", S.fromList ["alice", "charlie"])
  , ("charlie", S.fromList ["alice", "bob"])
  -- Community 2: Artists
  , ("diana", S.fromList ["eve", "frank", "alice"])
  , ("eve", S.fromList ["diana", "frank"])
  , ("frank", S.fromList ["diana", "eve"])
  -- Isolated user
  , ("gary", S.empty)
  ]

-- Network with an influencer (many followers, few following)
net5 :: Network
net5 = M.fromList
  [ ("influencer", S.singleton "celebrity")
  , ("alice", S.fromList ["influencer", "bob"])
  , ("bob", S.fromList ["influencer", "alice"])
  , ("charlie", S.fromList ["influencer"])
  , ("diana", S.fromList ["influencer", "eve"])
  , ("eve", S.fromList ["influencer"])
  , ("celebrity", S.empty)
  ]

-- Disconnected network (two separate components)
net6 :: Network
net6 = M.fromList
  [ ("alice", S.fromList ["bob"])
  , ("bob", S.fromList ["alice"])
  , ("charlie", S.fromList ["diana"])
  , ("diana", S.fromList ["charlie"])
  ]

-- Network for path finding tests
net7 :: Network
net7 = M.fromList
  [ ("a", S.fromList ["b", "c"])
  , ("b", S.fromList ["d"])
  , ("c", S.fromList ["d", "e"])
  , ("d", S.fromList ["f"])
  , ("e", S.fromList ["f"])
  , ("f", S.empty)
  ]

---------------------------------------------------------
-- Sample follow lists

follows1 :: [Follow]
follows1 = [("alice", "bob"), ("bob", "charlie"), ("charlie", "alice")]

follows2 :: [Follow]
follows2 = 
  [ ("alice", "bob"), ("alice", "charlie")
  , ("bob", "alice"), ("bob", "charlie")
  , ("charlie", "alice")
  ]

follows3 :: [Follow]
follows3 = 
  [ ("alice", "bob"), ("alice", "charlie"), ("alice", "diana")
  , ("bob", "alice"), ("bob", "charlie")
  , ("charlie", "alice"), ("charlie", "bob"), ("charlie", "diana"), ("charlie", "eve")
  , ("diana", "eve")
  , ("eve", "charlie"), ("eve", "diana")
  ]

---------------------------------------------------------
-- Expected results for testing

-- Metrics for "charlie" in net3
metricsCharlie3 :: Metrics
metricsCharlie3 = Metrics
  { followers = 3    -- alice, bob, eve follow charlie
  , following = 4    -- charlie follows alice, bob, diana, eve
  , mutuals = 2      -- alice and bob are mutual
  , influence = 0.75 -- 3/4
  }

-- Metrics for "influencer" in net5
metricsInfluencer5 :: Metrics
metricsInfluencer5 = Metrics
  { followers = 5    -- alice, bob, charlie, diana, eve follow influencer
  , following = 1    -- influencer only follows celebrity
  , mutuals = 0      -- no mutual connections
  , influence = 5.0  -- 5/1
  }

-- Friend recommendations for "alice" in net3
-- Should suggest "eve" (charlie follows eve, alice follows charlie)
recsAlice3 :: [Recommendation]
recsAlice3 = [("eve", 2)]  -- 2 paths: via charlie, via diana
