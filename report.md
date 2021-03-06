## Summary

We worked on a project that aimed to visualize the Ford Fulkerson max flow algorithm. In this process, we thought
deeply about appropriate data structures for graphs and networks, implementing the algorithm itself, and a good UX/UI. In addition to being able to visualize the Ford Fulkerson algorithm, our program includes a graph constructor that can be used to construct a rich set of networks through an intuitive UI. After constructing a graph, a user can run max flow on it as well as download it. As they build a graph, they can at any moment compute max flow on the current state of the graph, and then return to the graph constructor for further iteration. In addition to being able to download graphs, users can also upload graphs previously downloaded into the graph constructor.

## Directory Structure

All of the code is stored in the `src/` subdirectory. In this subdirectory, there is an `Index.elm` file and four further subdirectories: `Components/`, `Examples/`, `Logic/`, and `MVC/`. `Components/` stores all of the file relating to the UI, as well as some helper functions in the `Components/Utils/` folder. Each file in this directory corresponds to a different view. `Examples/` contains some example graphs, which can be uploaded into the graph constructor. `Logic/` contains the Ford-Fulkerson algorithm logic. Finally, `MVC/` contains the high-level files relating to types, state management, and a wrapper for the different views.

## Network Representations 

In order to represent a network, we use several representations. The first represesentation is an adjacency list packed in a record with some metadata, represented as a dictionary of dictionaries using the Dict library. The second representation is a list of vertices and list of edges containing coordinates. The final representation is the same as the first representation except it separates vertices at the top level (the outer Dict) by column with a List. This helps with ensuring operations on the graph constructor page work as expected.

## Ford Fulkerson Algorithm

We implement the standard Ford Fulkerson algorithm with depth first search, except in a functional method. `ford_fulkerson_helper` runs one round of Ford Fulkerson. It finds an augmenting path in the residual network using `augmenting`, which uses `dfs`, and then pushes flow along this augmenting path. If there is not augmenting path, the input network is simply returned. For more information, please see `src/Logic/Network.elm`.

## Visualization and Graph Constructor 

For the visualization, we employ a topological sort with Kahn's algorithm on each column of vertices to ensure there are minimal crossed edges in the graph visualization.

We also use Bootstrap (elm-bootstrap) to assist with basic UI components such as buttons.

In the graph constructor, there are 4 operations: 
1) Clicking on edge will add another vertex along that edge.
2) Holding shift and clicking on an edge will increment the capacity along that edge in the direction of the arrow by 1.
3) Dragging from vertex A to vertex B will move vertex A into the column of vertex B.
4) Holding shift and dragging from vertex A to vertex B will add an edge from vertex A to vertex B, provided theses vertices are at most one column apart.

With these four operations, you can create a lot of different kinds of flow networks.

## File Upload/Download

In order to upload and download graph representations, we encode the graph constructor representation into JSON with the JSON module and then download it with the File module. We again use these two modules for file uploads. For more details, please see `src/Components/Utils/GraphEncoder.elm`.


