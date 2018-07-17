#' Find simplicial nodes
#'
#' This function finds all simplicial nodes in a graph. Notice that isolated nodes are treated as
#' simplicial nodes for programming convenience. The output is a vector of node names, ordered by
#' node degree ascending.
#' @param graph A graph that is stored in an adjacency matrix.
#' @keywords simplicial
#' @export

find_simplicial = function(graph) {

  nodes = colnames(graph)
  sim = c()
  degrees = c()
  for (x in nodes) {

    nbrs = names(which(graph[x, ] == 1))
    if (length(nbrs) < 1) {

      sim = c(sim, x)
      degrees = c(degrees, 0)

    } else {

      if (is_simplicial(graph, x)) {

        sim = c(sim, x)
        degrees = c(degrees, length(nbrs))

      }

    }

  }

  if (length(sim) > 1) sim = sim[order(degrees)]

  return(sim)

}
