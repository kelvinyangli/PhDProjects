#' Random graph generation
#'
#' This function uses the rand_bounded_deg_graph_aux() to generate a random undirected graph for a
#' given maximum degree. The maximum degree may not be reached. This function uses try() to catch
#' error when there is no consistent DAG for a sampled degree sequence. There is always an output
#' from this function.
#' @param n The number of nodes in the graph.
#' @param delta The maximum degree in the graph.
#' @keywords random graph, bounded degree
#' @export

rand_bounded_deg_graph = function(n, delta) {

  g <- NULL
  while(is.null(g)) {
    try(
      g <- rand_bounded_deg_graph_aux(n, delta),
      silent = TRUE # omit error msg
    )
  }
  return(g)

}

