make_moral_prior = function(mr, ed) {

  indx = sample(which(as.vector(upper.tri(mr)) == TRUE), ed, replace = F)
  # remove/add ed random edges, depending on whether or not an edge in a
  # particular sampled entry exists
  m = as.vector(mr)
  for (ind in indx) {
    if (m[ind] == 0) {
      m[ind] = 1
    } else {
      m[ind] = 0
    }
  }
  mr2 = matrix(m, nrow = nvars, ncol = nvars, byrow = F)
  mr2[lower.tri(mr2)] = t(mr2)[lower.tri(mr2)]
  return(mr2)

}

