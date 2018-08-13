#' Calculate the shortest cycle length
#'
#' This function calculate the length of a shortest cycle between two nodes. If these nodes
#' don't form a cycle, the function returns length 0.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param x A node.
#' @param y A node.
#' @details The function has dependency on igraph.
#' @keywords shortest cycle
#' @export

calculate_cycle_length = function(graph, x, y, connected = FALSE) {

  l = 0 # to store length of the cycle if there is one, 0 indicates no cycle
  # if x and y are known to be directly connected by an edge
  if (connected) {

    # remove the edge x-y from graph
    graph = wrsgraph::subgraph(graph, from = x, to = y, type = "edges")
    # convert adjacency matrix to igraph's graph format
    g = igraph::graph_from_adjacency_matrix(graph)
    # distances of the shortest paths
    dist = igraph::distances(g, x, y)[1]
    # if there is another simple path between x and y then they form a cycle
    # length of the smallest cycle = dist + 1
    if (!is.infinite(dist)) l = dist + 1

  } else {# if x and y are not known to be connected or not

    # convert adjacency matrix to igraph's graph format
    g = igraph::graph_from_adjacency_matrix(graph)
    # find a shortest path
    spath = suppressWarnings(igraph::shortest_paths(g, x, y)$vpath[[1]])
    if (length(spath) > 0) {# if there is at least a path

      l = length(spath) - 1
      # remove the path from graph
      for (i in 1:l) {

        graph = wrsgraph::subgraph(graph, from = spath[i], to = spath[i + 1], type = "edges")

      }
      g = igraph::graph_from_adjacency_matrix(graph)
      # find a shortest path
      spath = suppressWarnings(igraph::shortest_paths(g, x, y)$vpath[[1]])
      if (length(spath) > 0) {# if there is another path

        l = l + length(spath) - 1

      } else {# else x and y don't form a cycle

        l = 0

      }

    }

  }

  return(l)

}



