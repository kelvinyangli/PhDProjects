#' Identify a clique
#'
#' This function tests if a set of nodes form a clique. A clique is a complete (fully connected) graph. The
#' output of the function is a boolean variable with a value 0 or 1.
#' @param graph A graph that is stored in an adjacency matrix.
#' @param nodes A set of nodes that are expressed in texts, e.g. c("V1", "V2")
#' @keywords clique
#' @export

is_clique = function(graph, nodes) {

  clq = 1
  for (i in 1:(length(nodes) - 1)) {

    if (clq == 0) break
    x = nodes[i]
    for (j in (i + 1):length(nodes)) {

      y = nodes[j]
      if (graph[x, y] != 1) {

        clq = 0
        break

      }

    }

  }

  return(clq)

}
