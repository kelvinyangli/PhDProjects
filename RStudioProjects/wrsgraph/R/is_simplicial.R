#' Identify a simplicial node
#'
#' This function tests if a given node is a simplicial node in a graph. A simplicial node is a node, whose
#' neighbours form a clique. The output of the function is a boolean variable with a value 0 or 1.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param node A node that is expressed in text, e.g. "V1" or "X1".
#' @keywords simplicial
#' @export


is_simplicial = function(graph, node) {

  nbrs = names(which(graph[node, ] == 1))
  if (length(nbrs) < 2) {

    clq = 1

  } else {

    clq = wrsgraph::is_clique(graph, nbrs)

  }

  return(clq)

}
