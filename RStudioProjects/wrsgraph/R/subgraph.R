#' Subgraph
#'
#' This function outputs a subgraph of a graph by either removing nodes or edges.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param nodes A set of nodes to be removed. Default is the empty set.
#' @param from A set of nodes that correspond to one side of the end nodes of the edges that will be
#' removed. Default is the empty set.
#' @param to A set of nodes that correspond to the other side of the end nodes of the edges that will
#' be removed. Default is the empty set.
#' @param type An argument to specify removing either nodes or edges from the original graph. Only three
#' strings are taken. That is, "nodes", "edges" or "cliques".
#' @keywords subgraph
#' @export

subgraph = function(graph, nodes = NULL, from = NULL, to = NULL, type = c("nodes", "edges", "clique")) {

  if (type == "nodes") {

    nodeIndex = which(colnames(graph) %in% nodes)
    graph = graph[-nodeIndex, -nodeIndex]

  } else if (type == "edges") {

    if (is.null(nodes)) {

      for (x in from) {

        for (y in to) {

          graph[x, y] = graph[y, x] = 0

        }

      }

    } else {

      for (x in nodes) {

        for (y in nodes[nodes != x]) {

          graph[x, y] = 0

        }

      }

    }

  } else if (type == "clique") {

    # remove the edge b/w all pairs of nodes
    for (x in nodes) {

      for (y in nodes[nodes != x]) {

        graph[x, y] = 0

      }

    }
    # remove isolated nodes
    isoNodesIndices = which(sapply(colnames(graph), wrsgraph::degree, graph = graph) == 0)
    graph = graph[-isoNodesIndices, -isoNodesIndices]

  }

  return(graph)

}
