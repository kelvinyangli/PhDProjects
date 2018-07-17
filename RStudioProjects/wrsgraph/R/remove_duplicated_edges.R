#' A function to remove duplicated rows in a dataframe or matrix
#'
#' This function removes from a dataframe or matrix duplicated rows. The dataframe or matrix stores
#' edges of a graph. The dataframe or matrix has two columns, which are the two end points of an edge.
#' @param x A dataframe or row.
#' @export

remove_duplicated_edges = function(x) {

  # order the two nodes by ascending
  for (i in 1:nrow(x)) x[i, ] = x[i, ][order(x[i, ])]

  # remove duplications
  if (is.data.frame(x)) {

    x = x[!duplicated.data.frame(x), ]

  } else if (is.matrix(x)) {

    x = x[!duplicated.matrix(x), ]

  }

  # omit row names
  row.names(x) = c()
  return(x)

}
