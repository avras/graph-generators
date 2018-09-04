{-|
  Implementations of random graphs with small-world properties, as described
  by Watts and Strogatz

  See Watts, D.J. and Strogatz, S.H.. Collective dynamics of `small-world'
  networks, Nature, 393, pp 440-442, (1998)

  graph-generators copyright:
    Copyright (C) 2014-2018 Uli Köhler

  NetworkX copyright:
    Copyright (C) 2004-2018 by 
    Aric Hagberg <hagberg@lanl.gov>
    Dan Schult <dschult@colgate.edu>
    Pieter Swart <swart@lanl.gov>
    All rights reserved.
    BSD license.
-}
module Data.Graph.Generators.Random.WattsStrogatz (
  -- ** Graph generators
        wattsStrogatzGraph,
        wattsStrogatzGraph',
    )
    where

import System.Random.MWC
import Control.Monad
import Data.Graph.Generators
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.Set as Set

{-|
    Generate a unlabelled undirected random graph using the Algorithm introduced by
    WattsStrogatz.

    Note that self-loops with also be generated with probability p.

    This algorithm runs in O(kn).

    The generated nodes are identified by [0..n-1].

    Example usage, using a truly random generator:
    
    > import System.Random.MWC
    > gen <- withSystemRandom . asGenIO $ return
    > wattsStrogatzGraph gen 1000 10 0.6
    ...
    
    Modelled after NetworkX 2.1 watts_strogatz_graph().
-}
wattsStrogatzGraph :: GenIO  -- ^ The random number generator to use
           -> Int    -- ^ n, The number of nodes
           -> Int    -- ^ k, the size of the neighborhood / degree (should be even)
           -> Double -- ^ \beta, The probability of a forward edge getting rewritten
           -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
wattsStrogatzGraph gen n k p = do
  let allNodes = [0..n-1]
      k' = k `div` 2
      edgesInRewriteOrder = concat $ fmap forwardEdgeList [1..k'] where
        forwardEdgeList i = zip allNodes $ fmap (\j -> j+i `mod` n) allNodes
      initialEdgeSet = Set.fromList edgesInRewriteOrder
  finalEdgeSet <- rewireAllEdges initialEdgeSet edgesInRewriteOrder
  return $ GraphInfo n (Set.toList finalEdgeSet)
  where
    rewireAllEdges :: Set.Set (Int, Int) -> [(Int, Int)] -> IO (Set.Set (Int, Int))
    rewireAllEdges edgeSet [] = return edgeSet
    rewireAllEdges edgeSet (t:tuples) = do
      r <- uniform gen :: IO Double
      if (r > p)
        then rewireAllEdges edgeSet tuples
        else do es <- rewireEdge t edgeSet
                rewireAllEdges es tuples

    rewireEdge :: (Int, Int) -> Set.Set (Int, Int) -> IO (Set.Set (Int, Int))
    rewireEdge (i, j) edgeSet = do
      j' <- uniformR (0, n-1) gen
      if (((Set.member (i, j') edgeSet) || (Set.member (j', i) edgeSet)) || (i == j')) 
        then rewireEdge (i, j) edgeSet
        else return $ Set.insert (i, j') $ Set.delete (i, j) edgeSet

{-|
    Like 'wattsStrogatzGraph', but uses a newly initialized random number generator.

    See 'System.Random.MWC.withSystemRandom' for details on how the generator is
    initialized.

    By using this function, you don't have to initialize the generator by yourself,
    however generator initialization is slow, so reusing the generator is recommended.

    Usage example:

    > wattsStrogatzGraph' 1000 10 0.6
-}
wattsStrogatzGraph' :: Int    -- ^ n, The number of nodes
                 -> Int    -- ^ k, the size of the neighborhood / degree (should be even)
                 -> Double -- ^ \beta, The probability of a forward edge getting rewritten
                 -> IO GraphInfo -- ^ The resulting graph (IO required for randomness)
wattsStrogatzGraph' n k p =
    withSystemRandom . asGenIO $ \gen -> wattsStrogatzGraph gen n k p
