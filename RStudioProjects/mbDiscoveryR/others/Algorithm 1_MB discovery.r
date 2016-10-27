mbCMI = function(data, x, beta, n, alpha = 0.05, debug = FALSE) {
  nodes = names(data)
  cpc = csp = c()
  cmb = c(cpc, csp)
  result.previous = result.current = c()
  # cmi 
  cmi.previous = cmi.current = c()
  # p.value 
  association.previous = association.current = c()
  
  # function to compute cmi and pvalue 
  cmi2 = function(x, cmb = NULL, data) {
    var = names(data)
    xz = c(x, cmb)
    vec.cmi = rep(NA, length(var))
    vec.pvalue = rep(NA, length(var))
    # compute cmi and pvalue
    for (i in 1:length(var)) {
      if (!var[i] %in% xz) {
        if (is.null(cmb)) {
          result.temp = ci.test(x = x, y = var[i], data = data)
          vec.cmi[i] = result.temp$statistic
          vec.pvalue[i] = result.temp$p.value
        } else {
          result.temp = ci.test(x = x, y = var[i], z = cmb, data = data)
          vec.cmi[i] = result.temp$statistic
          vec.pvalue[i] = result.temp$p.value
        }
      }
    }
    # store results in data.frame
    df = data.frame(vec.cmi, vec.pvalue)
    colnames(df) = c("cmi", "pvalue")
    return(df)
  }
  
  if (debug) {
    cat("----------------------------------------------------------------\n")
    cat("* learning the Markov blanket of", x, "\n")
    cat("    * calculating CMI \n")
  } # then
  
  # initial step
  result.current = cmi2(x = x, data = data)
  cmi.current = result.current$cmi
  association.current = result.current$pvalue
  
  if (debug) {
    for (i in 1:length(nodes)) cat("    >", nodes[i], "has CMI:", cmi.current[i], "\n")  
  } # then
  
  to.add = nodes[which.max(cmi.current)]
  cpc = c(cpc, to.add)
  cmb = c(cmb, cpc, csp)
  
  if (debug) {
    cat("    @", to.add, "include in the parents and children set", "\n")
    cat("    > Markov blanket (", length(cmb), "nodes ) now is '", cmb, "'\n")
  } # then
  
  
  # following steps
  repeat {
    cmi.previous = cmi.current 
    association.previous = association.current
    result.current = cmi2(x = x, cmb = cmb, data = data)
    cmi.current = result.current$cmi
    association.current = result.current$pvalue
    # stop if there are no candiate for inclusion 
    if(association.current[which.max(cmi.current)] > alpha) break
    
    if (debug) {
      cat("    * calculating CMI \n")
      for (i in 1:length(nodes)) cat("    >", nodes[i], "has CMI:", cmi.current[i], "\n")  
    } # then
    
    to.add = nodes[which.max(cmi.current)]
    # check for cmi changes
    cmi.change = cmi.current/cmi.previous
    # select nodesiables whose cmi increased >= beta times
    top.increase = nodes[which(cmi.change >= beta)]
    # check if these top increasing nodes are in top 5 of cmi
    if (!length(top.increase) == 0) {
      # order current cmi by decreasing 
      cmi.current.order = order(cmi.current, decreasing = TRUE)
      nodes.de = nodes[cmi.current.order]
      cmi.current.de = cmi.current[cmi.current.order]
      for (i in 1:length(top.increase)) {
        if (which(nodes.de == top.increase[i]) <= n) {
          csp = c(csp, top.increase[i])
          if (debug) {
            cat("    @", top.increase[i], "include in the spouses set", "\n")
          } # then
        }
      }
    }
    # check if too.add belongs to csp
    if (!to.add %in% top.increase) {
      cpc = c(cpc, to.add)
      
      if (debug) {
        cat("    @", to.add, "include in the parents and children set", "\n")
      } # then
    }
    cmb = unique(c(cmb, cpc, csp))
  }
  
  # delete false positives 
  for (y in cmb) {
    if (debug)
      cat("* checking node", y, "for exclusion.\n")
    pvalue.y = ci.test(x = x, y = y, z = cmb[cmb != y], data = data)$p.value
    if(pvalue.y > alpha) {
      if (y %in% cpc) {
        cpc = cpc[cpc != y]
      } else {
        csp = csp[csp != y]
      }
      cmb = unique(c(cpc, csp))
      
      if (debug) {
        cat("    > node", y, "removed from the markov blanket. ( p-value:", pvalue.y, ")\n")
        cat("    > Markov blanket (", length(cmb), "nodes ) now is '", cmb, "'\n")
      } # then
    }
  }
  
  # store results in list 
  lst = list(cmb,cpc, csp)
  names(lst) = c("mb","pc","sp")
  
  if (debug) {
    cat("* Algorithm stopped", "'\n")
    cat("    > Markov blanket (", length(cmb), "nodes ) is '", cmb, "'\n")
    cat("    > spouses set is '", csp, "'\n")
  } # then
  
  return(lst$mb)
}

