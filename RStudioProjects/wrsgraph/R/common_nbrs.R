#' A function that finds the common neighbours between two nodes
#'
#' This function finds the common neighbours between two nodes from a graph. It
#' returns an empty vector of length 0 if there is no common neighbours.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param x A node in graph.
#' @param y A node in graph that is different from x.
#' @export

common_nbrs = function(graph, x, y) {

  xNbrs = names(which(graph[x, ] == 1))
  yNbrs = names(which(graph[y, ] == 1))
  return(intersect(xNbrs, yNbrs))

}
