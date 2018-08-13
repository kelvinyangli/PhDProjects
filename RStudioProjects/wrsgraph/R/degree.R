#' A function to calculates the degree of a node
#'
#' @param graph A graph that is stored in an adjacency matrix.
#' @param node A node that is expressed in text, e.g. "V1" or "X1".
#' @keywords degree
#' @export

degree = function(graph, node) {

  nbrs = find_nbr(graph, node)
  return(length(nbrs))

}
