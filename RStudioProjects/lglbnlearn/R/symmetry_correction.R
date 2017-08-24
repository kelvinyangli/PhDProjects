#' A function to enforce the symmetry correction after Markov blankets have been learned 
#'
#' This function enforces the symmetry check for each learned mb. That is, if a node x is is mb(y), but y isn't
#' in mb(x), then either add y into mb(x) or delete x from mb(y), depending on which deterministic rule to 
#' follow. The two famous deterministic rules are the AND and OR rules. For MB discovery using MML, we prefer
#' to use the AND rule due to the high precision and low recall of mml_cpt. 
#' @param vars A vector of all variables. 
#' @param mbList A list of learned Markov blankets.
#' @param rule A string argument takes either "AND" or "OR", indicating the two deterministic rules mentioned
#' above. 
#' @export
symmetry_correction = function(vars, mbList, rule) {
  for (i in 1:length(vars)) {# for each node 
    # for each learned MB, find the MBs that contain x
    for (j in 1:length(mbList)) {
      if (vars[i] %in% mbList[[j]]) {# if i in mb(j)
        if (!vars[j] %in% mbList[[i]]) {# if j is not in mb(i)
          if (rule == "AND") {# add i into mb(j) for AND rule
            mbList[[i]] = c(mbList[[i]], vars[j])
          } else if (rule == "OR") {# delete j from mb(i) for OR rule
            mbList[[j]] = mbList[[j]][-which(mbList[[j]] == vars[i])]
          }
        }
      }
    }# end for each learned mb
  }# end for each node 
  return(mbList)
}


