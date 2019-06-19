module Dijkstra where

type Distance = Integer

type Node = String
type Edge = (Node, Distance, Node)
type Graph = ([Node], [Edge])

data State = Unvisited | Discovered Distance | Finalized Distance deriving Show

type Result = [(Node, [(Node, Distance)])]

getState ::  Node -> [(Node, State)] -> State
getState node nodesWithStates =
  case lookup node nodesWithStates of
    Nothing -> error $ "Unable to get state of node: " ++ node
    Just state -> state

setState :: Node -> State -> [(Node, State)] -> [(Node, State)]
setState node state nodesWithStates =
  let
    update :: (Node, State) -> (Node, State)
    update (n, s) = if n == node then (node, state) else (n, s)
  in
    map update nodesWithStates

closestDiscovered :: [(Node, State)] -> Maybe (Node, Distance)
closestDiscovered =
  let
    closest :: (Node, State) -> Maybe (Node, Distance) -> Maybe (Node, Distance)
    closest (n, Discovered d) Nothing = Just (n, d)
    closest (n, Discovered d) (Just (n', d')) =
      if d < d' then (Just (n, d)) else (Just (n', d'))
    closest current accumulator = accumulator
  in
    foldr closest Nothing

testState = [("a", Unvisited), ("b", Discovered 10), ("c", Finalized 5)]

mergeStates :: State -> State -> State
mergeStates Unvisited (Discovered d) = Discovered d
mergeStates (Discovered d) (Discovered d') = Discovered (min d d')
mergeStates state _ = state

updateState :: (Node, State) -> [(Node, State)] -> [(Node, State)]
updateState (node, state) nodesWithStates =
  let
    currentState = getState node nodesWithStates
    mergedState = mergeStates currentState state
  in
    setState node mergedState nodesWithStates

shortestPathDistances' :: [Edge] -> [(Node, State)] -> [(Node, State)]
shortestPathDistances' edges nodesWithStates =
  case closestDiscovered nodesWithStates of
    Nothing -> nodesWithStates
    Just (node, distance) ->
      let
        currentEdges =
          filter (\(f, d, t) -> f == node) edges

        potentialUpdates =
          map (\(f, d, t) -> (t, Discovered (distance + d))) currentEdges

        updatedState =
          foldr updateState nodesWithStates potentialUpdates

        finalizedState =
          setState node (Finalized distance) updatedState
      in
        shortestPathDistances' edges finalizedState

startState :: Node -> [Node] -> [(Node, State)]
startState node nodes =
  map (\n -> if n == node then (n, Discovered 0) else (n, Unvisited)) nodes

shortestPathDistances :: Node -> Graph -> [(Node, Distance)]
shortestPathDistances from graph@(nodes, edges) =
  let
    nodesWithStates = (shortestPathDistances' edges (startState from nodes))

    getDistance :: (Node, State) -> (Node, Distance)
    getDistance (n, Finalized d) = (n, d)
    getDistance (n, _) = error $ "No path found from " ++ from ++ " to " ++ n
  in
    map getDistance nodesWithStates
