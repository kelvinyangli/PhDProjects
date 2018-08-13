#' Random graph generation (auxiliary)
#'
#' This is an auxiliary function of the rand_bounded_deg_graph() function. It may return an error due to
#' non-existence of a consistent DAG for a sampled degree sequence. It first generates a random
#' degree sequence based on the maximum degree, then use the igraph::sample_degseq(out.deg, method = "vl")
#' function to generate a connected undirected random graph consistents with the sampled degree sequence.
#' @param n The number of nodes in the graph.
#' @param delta The maximum degree in the graph.
#' @keywords random graph, bounded degree
#' @export

rand_bounded_deg_graph_aux = function(n, delta) {

  repeat {
    degSeq = sample(delta, n, replace = T)
    if (sum(degSeq) %% 2 == 0) break # ensure the sum of the degrees is even number
  }

  # randomly generate an undirected graph with the given deg seq
  # the "vl" method guarantees a connected graph
  g = sample_degseq(degSeq, method = "vl")
  m = as.matrix(as_adjacency_matrix(g)) # convert igraph graph into adjacency matrix
  varNames = paste0("V", 1:n)
  dimnames(m) = list(varNames, varNames)
  return(m)

}





