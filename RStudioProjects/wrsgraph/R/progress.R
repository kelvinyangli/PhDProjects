#' A function to keep track of the progress in a loop
#'
#' The function keeps track of the looping progress.
#' @param i Index in a loop.
#' @param q Separation, e.g., only show every 100 indices.
#' @param n Total number of iterations.
#' @export

progress = function(i, q, n) {
  if (i == 1) {
    print(i)
  } else if (((i %% q) == 0) && (i < n)) {
    print(i)
  } else if (i == n) {
    print("Done!")
  }
}
