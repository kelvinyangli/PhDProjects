#' Definite-WRS for maximum degree 4 graphs
#'
#' This function is a polynomial time algorithm for checking the morality of
#' undirected graphs with maximum degree 4. The algorithm has been proved to be
#' correct. It either removes a simplicial node or a simplicial clique, depending
#' on situations like node degree, and K3 stack length etc.
#' @param G A given undirected graph stored in an adjacency matrix format.
#' @param debug A boolean argument to show steps of the backtracking algorithm.
#' @export

d_wrs_deg4 = function(G, debug = F) {

  # return T if G is chordal
  if (wrsgraph::is_chordal(G)) {

    if (debug) cat("chordal \n")
    return(wrs = 1)

  }

  # prune all leave in G
  # since chordality is check before prunning leaves
  # the graph after pruning is guaranteed to be non-empty
  if (debug) cat("prune leaves \n")
  G = prune_leaves(G)

  # find all sim nodes in G
  # after prune, all simplicial nodes have at least 2 nbrs
  if (debug) cat("find sims \n")
  sim = find_simplicial(G)

  simNbrList = list()
  nK3s = c()
  # 0. caculate length of k3 stacks and nbrs of sim nodes
  if (length(sim) > 0) {
    # count nbrs for each sim node
    simNbrList = lapply(sim, wrsgraph::find_nbr, graph = G)
    names(simNbrList) = sim
    # calculate the length of a stack of k3s, ranges from 1 to many
    nK3s = rep(0, length(sim))
    for (i in 1:length(sim)) {
      nK3s[i] = k3_ladder(G, sim[i], simNbrList[[i]][1], simNbrList[[i]][2], 1)
    }
  }

  # 1. k4+... or k5
  ind = which(sapply(simNbrList, length) > 2)
  if (length(ind) > 0) {
    if (debug) cat("step 1 \n")
    i = ind[1]
    G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
    res = d_wrs_deg4(G)
    if (res == 1) {
      return(wrs = 1)
    } else {
      return(wrs = 0)
    }
  }

  # 2. k3+{k3,k4,2*k3}
  ind = which(nK3s %in% c(-1, 2))
  if (length(ind) > 0) {
    if (debug) cat("step 2 \n")
    i = ind[1]
    G = wrsgraph::subgraph(G, nodes = sim[i], type = "nodes")
    res = d_wrs_deg4(G)
    if (res == 1) {
      return(wrs = 1)
    } else {
      return(wrs = 0)
    }
  }

  # 3. k3+{stack of 3 k3s, cm, share no edge}
  ind = which(nK3s %in% c(0, 1))
  if (length(ind) > 0) {
    if (debug) cat("step 3 \n")
    i = ind[1]
    G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
    res = d_wrs_deg4(G)
    if (res == 1) {
      return(wrs = 1)
    } else {
      return(wrs = 0)
    }
  }

  # all the sim nodes left are in length >= 3 stack of k3s
  #
  # 4.0 deal with cases when a stack has length > 3
  # in this case, remove a simplicial k3
  ind = which(nK3s > 3)
  if (length(ind) > 0) {
    if (debug) cat("step 4.0 \n")
    i = ind[1]
    G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
    res = d_wrs_deg4(G)
    if (res == 1) {
      return(wrs = 1)
    } else {
      return(wrs = 0)
    }
  }

  # 4.1 order k3 stacks so that sp is dealt at last
  ind = which(nK3s == 3)
  if (length(ind) > 0) {
    if (debug) cat("step 4.1 \n")
    ind = c()
    for (i in 1:length(sim)) {
      mtch = matching_special_envelop_graph(G, sim[i])
      if (mtch == 1) ind = c(ind, i)
    }

    # in the current graph, if there are two sim nodes in the same stack
    # the only possibility is that they are both in the special sp graph
    # if this is the case, they will be recorded in ind by the subgraph
    # matching process, hence the rest sim nodes are safely to be removed
    # together, since they aren't in the same stack
    # disconnected graph? a stack of 3 k3s?
    if (length(ind) < length(sim)) {
      if (length(ind) > 0) {
        sim = sim[-ind]
        simNbrList = simNbrList[-ind]
        nK3s = nK3s[-ind]
      }
      i = 1
      G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
      res = d_wrs_deg4(G)
      if (res == 1) {
        return(wrs = 1)
      } else {
        return(wrs = 0)
      }
    } else {
      # when all sim nodes are in special sp graph
      if (length(sim) > 1) {
        # check for two nodes in the same stack case
        ind = c()
        for (i in 1:(length(sim) - 1)) {
          for (j in (i + 1):length(sim)) {
            # since max.deg=4, there is max 1 common nbr in the rest of the graph
            # so remove both sim nodes
            if (length(intersect(simNbrList[[i]], simNbrList[[j]])) == 1) {
              ind = c(ind, i, j)
            }
          }
        }
        # 4.1 if two are in the same stack, remove both sim nodes
        if (length(ind) > 0) {
          G = wrsgraph::subgraph(G, nodes = sim[ind], type = "nodes")
          res = d_wrs_deg4(G)
          if (res == 1) {
            return(wrs = 1)
          } else {
            return(wrs = 0)
          }
        } else {
          # remove a random sim k3
          i = 1
          G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
          res = d_wrs_deg4(G)
          if (res == 1) {
            return(wrs = 1)
          } else {
            return(wrs = 0)
          }
        }
      } else {# when there is only 1 sim node left in a stack of 3 k3s
        # remove the sim k3
        i = 1
        G = wrsgraph::subgraph(G, nodes = c(sim[i], simNbrList[[i]]), type = "clique")
        res = d_wrs_deg4(G)
        if (res == 1) {
          return(wrs = 1)
        } else {
          return(wrs = 0)
        }
      }
    }
  }

  return(wrs = 0)

}
