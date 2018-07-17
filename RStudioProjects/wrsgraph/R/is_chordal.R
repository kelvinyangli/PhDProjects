#' Test chordality
#'
#' This function tests if a given graph is a chordal graph. A chordal graph is an undirected graph
#' such that any cycle of length greater than 3 must have a chord. It has dependency on the function
#' is_chordal in the igraph package.
#' @param graph A graph that is stored in an adjacency matrix.
#' @keywords chordal
#' @export

is_chordal = function(graph) {

  ig = igraph::graph_from_adjacency_matrix(graph, mode = "undirected")
  return(as.numeric(igraph::is_chordal(ig)$chordal))

}
