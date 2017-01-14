-- Graph Representation
-- The graph is represented by an edge list - each vertex has connection with
-- one or more vertexes. These connections are packed in tuples.
-- More info on the topic: https://www.khanacademy.org/computing/computer-science/algorithms/graph-representation/a/representing-graphs

type Vertex = Int
type Edge = (Vertex, Vertex)
type Graph = [Edge]

findVertexEdges :: Graph -> Vertex -> [Edge]
findVertexEdges graph vertex = helper graph vertex []
  where
    helper :: Graph -> Vertex -> [Edge] -> [Edge]
    helper [] _ edges = edges
    helper (edge:graph) vertex edges
      | (fst edge) == vertex = (helper graph vertex (edges ++ [edge]))
      | otherwise = (helper graph vertex edges)

isSumReachable :: Graph -> Vertex -> Int -> Bool
isSumReachable graph v n = (helper graph (findVertexEdges graph v) v n False)
  where
    helper :: Graph -> [Edge] -> Vertex -> Int -> Bool -> Bool
    helper _ [] _ _ _ = False
    helper graph edges currentSum desiredSum nonTrivial
      | nonTrivial && currentSum == desiredSum = True
      | otherwise = not (null (filter (\x -> x == True) (map (\edge -> (helper graph (findVertexEdges graph (snd edge)) (currentSum + (snd edge)) desiredSum True)) edges)))

main = do
  (print (isSumReachable graph 2 7))
  (print (isSumReachable graph 1 8))
  (print (isSumReachable graph 1 10))
  (print (isSumReachable graph 6 6))
  (print (isSumReachable graph 6 3))

  where
    graph :: Graph
    graph = [(1, 2), (1, 3), (1, 4), (2, 5), (3, 6), (4, 5), (5, 6)]
