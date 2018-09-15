#' Definite-WRS for maximum degree 3 graphs
#'
#' This function is a polynomial time algorithm for checking the morality of
#' undirected graphs with maximum degree 3. The algorithm has been proved to be
#' correct. It finds a simplicial clique (i.e., a simplicial node
#' and its neighbours), and removes the entire clique.
#' @param G A given undirected graph stored in an adjacency matrix format.
#' @param debug A boolean argument to show steps of the backtracking algorithm.
#' @export

d_wrs = function(G, debug = F) {

  # return T if G is chordal
  if (wrsgraph::is_chordal(G)) {

    if (debug) cat("chordal \n")
    return(wrs = 1)
    #return(list(wrs = 1, dag = D))

  }

  # prune all leave in G
  # since chordality is check before prunning leaves
  # the graph after pruning is guaranteed to be cyclic with at least 4 nodes
  G = prune_leaves(G)

  # find all sim nodes in G
  sim = find_simplicial(G)

  if (length(sim) > 0) {

    for (node in sim) {# check if it's a sim node

      if (node %in% colnames(G)) {

        nbrs = names(which(G[node, ] == 1))
        # since G has been pruned, the min # of nbrs >= 2
        # since only max deg 3 graphs are guaranteed to be d-wrs
        # the max # nbrs <= 3
        #... remove the sim clique
        # the isolated nodes are also removed by subgraph() function
        G = wrsgraph::subgraph(G, nodes = c(node, nbrs), type = "clique")
        # orient D
        # D[node, nbrs] = 0
        #}

      }


    }# end for each node

    # apply wrs_bktr recursively
    res = d_wrs(G, debug)
    if (res == 1) {

      return(wrs = 1)
      #D = res$dag
      #return(list(wrs = 1, dag = D))

    }

  }

  return(wrs = 0)

}

