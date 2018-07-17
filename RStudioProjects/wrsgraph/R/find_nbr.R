#' A function that finds neighbours of a node
#'
#' This function finds neighbours of a node in a graph, which is stored in a contigency matrix. The output
#' of the function are a vector of node names.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param node A node that is expressed in text, e.g. "V1" or "X1".
#' @keywords neighbour
#' @export


find_nbr = function(graph, node) names(which(graph[node, ] == 1))
