#' special subgraph matching
#'
#' This function searches for a special envelop subgraph in the given graph G.
#' The function returns 0 if there is not such a matching or 1 if there is or NA
#' if the given node is not in a stack of 3 K3s.
#' @param G A given undirected graph stored in an adjacency matrix format.
#' @param node A node that is in a stack of 3 K3s.
#' @export

matching_special_envelop_graph = function(G, node) {

  cycle = 0
  nbrs = find_nbr(G, node)
  x = nbrs[1]
  y = nbrs[2]
  z = common_nbrs(G, x, y)
  z = z[z != node]
  if (length(common_nbrs(G, z, y)) < 2) {
    # y and z form the middle edge e in the stack of 3
    e1 = y
    w = common_nbrs(G, x, z)
    w = w[w != y]
    exclNodes = c(node, x, w)
  } else {
    e1 = x
    w = common_nbrs(G, y, z)
    w = w[w != x]
    exclNodes = c(node, y, w)
  }
  e2 = z
  # check whether e1 and e2 form a cycle when removing exclNodes
  tempG = wrsgraph::subgraph(G, nodes = exclNodes, type = "nodes")
  if (calculate_cycle_length(tempG, e1, e2, connected = T) > 0) {
    cycle = 1
  }

  return(cycle)

}
