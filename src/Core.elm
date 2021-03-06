module Core exposing (..)
import Set exposing (Set(..))

-- section 2.2
type Graph a 
  = Empty 
  | Vertex a -- single vertex
  | Overlay (Graph a) (Graph a) -- union of two graphs
  | Connect (Graph a) (Graph a) -- union but edges added between graph vertices

type Relation comparable 
  = R (Set comparable) (Set (comparable, comparable)) -- set of vertices and set of edges 


-- examples
-- two vertices with an edge between them: Connect (Vertex 1) (Vertex 2)
-- three vertices forming a path: Connect (Vertex 1) (Overlay (Vertex 2) (Vertex 3))
-- a loop: Connext (Vertex 1) (Vertex 1)

curry : ((a,b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x,y)

uncurry : (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- connect edge between vertices, section 2.2
edge : a -> a -> Graph a 
edge x y = 
  Connect (Vertex x) (Vertex y)

edges : List (a, a) -> Graph a 
edges es = 
  (List.foldr Overlay Empty << List.map (uncurry edge)) es

-- construct graph of edges, without vertices, section 2.2
vertices : List a -> Graph a
vertices vs =
  List.foldr Overlay Empty <| List.map Vertex vs

-- clique: fully-connected graph, section 2.2
clique : List a -> Graph a 
clique vs = 
  List.foldr Connect Empty <| List.map Vertex vs

-- convert from adjacency list (V,E) to algebraic graph
graph : List a -> List (a,a) -> Graph a 
graph vs es = Overlay (vertices vs) (edges es)

equals : Graph a -> Graph a -> Bool
equals g1 g2 = 
  True 

relation_to_graph : Relation comparable -> Graph comparable 
relation_to_graph (R vs es) = 
  Overlay (vertices <| Set.toList vs) (edges <| Set.toList es)

graph_to_relation : Graph comparable -> Relation comparable 
graph_to_relation g = 
  let 
    as_relation : Set comparable -> Set (comparable, comparable) -> Graph comparable -> Relation comparable
    as_relation v_acc e_acc g1 = 
      case g1 of 
        Empty -> R v_acc e_acc 
        Vertex x -> R (Set.insert x v_acc) e_acc
        Overlay xs ys ->
          let 
            (R v1 e1) = as_relation Set.empty Set.empty xs 
            (R v2 e2) = as_relation Set.empty Set.empty ys
          in
            (R (Set.union v1 v2) (Set.union e1 e2)) 
        Connect xs ys -> 
          let 
            (R v1 e1) = as_relation Set.empty Set.empty xs 
            (R v2 e2) = as_relation Set.empty Set.empty ys

            more_edges : Set (comparable, comparable)
            more_edges = Set.fromList <| List.concatMap (\s -> List.map (\r -> (s,r)) (Set.toList e2)) (Set.toList e1)
          in
            R (Set.union v1 v2) (Set.union e1 e2 |> Set.union more_edges)
  in 
    as_relation Set.empty Set.empty g


isSubgraphOf : Graph a -> Graph a -> Bool
isSubgraphOf g1 g2 = Overlay g1 g2 == g2




tests =
  Vertex 1 == vertices [1]




-- TODO 
-- a function that can check if one graph is a subgraph of another 
-- a function that can take a graph and spit out a list of the values
-- a function that can compare equality of graphs
