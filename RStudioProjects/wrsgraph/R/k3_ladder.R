#' A function that look for a k3 ladder.
#'
#' This function finds the length of the k3 (triangle) ladder starts from the
#' given simplicial node. It returns the number of adjancet k3s. It also returns
#' the value -1 if the initial k3 shares an edge with either a k4 or two k3s,
#' and value 0 if the initial k3 and 3 other k3s form a triangle.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param s A simplicial node in the graph.
#' @param x One neighbour of the node s.
#' @param y The other neighbour of the node s.
#' @param l The number of k3s adjacent togethers. It starts from 1.
#' @export

k3_ladder = function(graph, s, x, y, l) {

  # look for common node b/w x and y
  # since the function looks for k3 ladders, it is only applied to cases
  # when the simplicial node has 2 nbrs, ie. a simplicial k3
  z = wrsgraph::common_nbrs(graph, x, y)
  # remove original sim node
  z = z[z != s]
  if (length(z) > 1) {
    # this is the case when x and y have more than 1 common nbr
    # in the case of max.deg=4, x and y can have at most 2 common nbrs
    # if there are two common nbrs, it means the k3 shares an edge w/
    # either a k4 or two k3s
    return(-1)
  } else if (length(z) > 0) {# if exists a common nbr
    # add one k3
    l = l + 1
    # remove s from graph
    graph = wrsgraph::subgraph(graph, nodes = s, type = "nodes")
    # apply recursion
    if (length(common_nbrs(graph, x, z)) < 2) {
      l = k3_ladder(graph, x, y, z, l)
    } else if (length(common_nbrs(graph, y, z)) < 2) {
      l = k3_ladder(graph, y, x, z, l)
    } else if (length(common_nbrs(graph, x, z)) +
               length(common_nbrs(graph, y, z)) == 4) {
      return(0)
    }
  }

  return(l)

}
