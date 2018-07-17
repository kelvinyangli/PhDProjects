#' Auxilliary function to wrs_bktr()
#'
#' @param i Index of the number of visits. i > 1.
#' @param n Total number of edges could be safely removed. n > 1.
#' @export

combn_ind = function(i, n) {

  ss = 0
  for (j in 0:n) {

    ss = ss + choose(n, j)
    if (ss >= i) break

  }

  ind = i - (ss - choose(n, j))
  return(combn(1:n, j)[, ind])

}
