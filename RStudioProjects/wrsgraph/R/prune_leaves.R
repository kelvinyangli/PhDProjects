#' A function that prunes leaves
#'
#' This function recursively removes leaves from a graph. A leave is a node with degree 1. The process
#' stops if there is no leaves or the graph is a single node or empty graph.
#' @param graph A graph that is stored in an adjacency matrix.
#' @export

prune_leaves = function(graph) {

  repeat {

    # stop when there are only two nodes with one edge connects them.
    if (length(graph) < 2) break
    nodes = colnames(graph)
    degrees = sapply(nodes, wrsgraph::degree, graph = graph)
    if (min(degrees) > 1) break
    graph = wrsgraph::subgraph(graph, nodes[which(degrees < 2)], type = "nodes")

  }
  return(graph)

}
