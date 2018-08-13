#' Test weak recursively simplicial
#'
#' This function tests if a given graph is weak recursively simplicial. If test positive, the given
#' undirected graph is also oriented into a hybrid graph (a graph may contain both undirected and
#' directed edges). This function uses backtracking method to test a graph, so it can be slow for large
#' graphs. It outputs positive if the graph is either chordal or wrs.
#' @param G A given undirected graph stored in an adjacency matrix format.
#' @param D An undirected graph to start orienting with. Coud be set to G.
#' @param debug A boolean argument to show steps of the backtracking algorithm.
#' @keywords weak recursively simplicial
#' @export

wrs_bktr = function(G, D, debug = F) {

  # return T if G is chordal
  if (is_chordal(G)) {

    if (debug) cat("chordal \n")
    return(list(wrs = 1, dag = D))

  }

  # find all sim nodes in G
  sim = find_simplicial(G)

  # proceed if G has sim nodes, else return F
  if (length(sim) > 0) {

    # a matrix to store all valid edges that can be removed
    nbrsNbrMtx = matrix(0, 0, 2)
    simNbrList = list()
    if (debug) cat("sim:", sim, "\n")
    #if (debug) cat("nbrs: \n")

    # for each sim node x...
    for (x in sim) {

      # ... find its nbrs in G
      xNbrs = find_nbr(G, x)
      # ... omit deleted sim
      xNbrs = xNbrs[!xNbrs %in% sim]
      # ... store them in a list
      simNbrList[[x]] = xNbrs
      # if (debug) {
      #   cat(x, "-", xNbrs, "\n")
      # }
      # ... orient D
      D[x, xNbrs] = 0

    }

    # list unique nbrs
    nbrs = unique(unlist(simNbrList))

    # # omit deleted sim
    # nbrs = nbrs[!nbrs %in% sim]

    # remove sim
    G = subgraph(G, nodes = sim, type = "nodes")
    for (x in nbrs) {

      for (i in 1:length(simNbrList)) {
        if (is.element(x, simNbrList[[i]])) {
          if (length(simNbrList[[i]]) > 1) {

            xNbrs = simNbrList[[i]]
            xNbrs = xNbrs[xNbrs != x]
            xNbrs = xNbrs[!xNbrs %in% sim]
            nbrsNbrMtx = rbind(nbrsNbrMtx, cbind(x, xNbrs))

          }
        }
      }
    }

    # remove duplicated edge cuts
    if (length(nbrsNbrMtx) > 0) nbrsNbrMtx = remove_duplicated_edges(nbrsNbrMtx)
    if (length(nbrsNbrMtx) == 2) nbrsNbrMtx = matrix(nbrsNbrMtx, 1, 2)

    if (debug) cat("#edges:", nrow(nbrsNbrMtx), "\n")

    if (nrow(nbrsNbrMtx) > 0) {

      # backup G
      backupG = G
      # backup D
      backupD = D

      indices = c()
      i = 1
      # remove each edge combination and apply wrs_bktr()
      while (length(indices) < nrow(nbrsNbrMtx)) {
        #if (debug) cat(".")
        # when i=1, remove nothing
        # when i > 1, remove edges
        if (i > 1) {

          # restore backup
          G = backupG
          D = backupD

          # compute indices
          indices = combn_ind(i, nrow(nbrsNbrMtx))

          # if (nrow(nbrsNbrMtx) > 1) {
          #   # compute indices
          #   indices = combn_ind(i, nrow(nbrsNbrMtx))
          # } else {# when there is only one option
          #   indices = i
          # }

          # iteratively remove edges
          for (ind in indices) {
            from = nbrsNbrMtx[ind, 1]
            to = nbrsNbrMtx[ind, 2]
            G = subgraph(G, from = from, to = to, type = "edges")
            D = subgraph(D, from = from, to = to, type = "edges")
          }

        }

        # apply wrs_bktr recursively
        res = wrs_bktr(G, D, debug)
        if (res$wrs == 1) {

          D = res$dag
          return(list(wrs = 1, dag = D))

        }

        i = i + 1

      }# end while
    } else {

      # apply wrs_bktr recursively
      res = wrs_bktr(G, D, debug)
      if (res$wrs == 1) {

        D = res$dag
        return(list(wrs = 1, dag = D))

      }
    }
  }

  return(list(wrs = 0))

}

