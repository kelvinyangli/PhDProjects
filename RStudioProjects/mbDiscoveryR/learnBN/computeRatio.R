computeRatio = function(targetNode, mb, data) {
  
  for (i in 2:length(mb)) {# starting from the 2nd variable, since the 1st has been
    # added to pc(targetNode)
    
    mi = ci.test(x = targetNode, y = mb[i], data = data)$statistic
    
    maxCMI = 0 # initialize cmi to 0
    condSet = c() # initialize conditioning set to empty 
    for (j in 1:length(mb)) {# but the 1st variable can be considered as a conditioning set
      
      if (j != i) {
        
        cmi = ci.test(x = targetNode, y = mb[i], z = mb[j], data = data)$statistic
        if (cmi > maxCMI) {
          
          maxCMI = cmi 
          condSet = j
          
        } # end if cmi > maxCMI
        
      } # end if j != i
      
    } # end for j
    
    # compare the ratio b/w maxCMI and mi to see if there is a dramatic increasing
    # in mutual information. This maxCMI, however, should also be a large value, since
    # if mi = 10e-6 and maxCMI = 10e-3, then the ratio is 1000, but the maxCMI is still
    # a small value, which means the dependece is still quite weak. 
    ratio = maxCMI / mi
    cat(paste0(targetNode, "-", mb[condSet], "-", mb[i]), "& mi:", round(mi, 2), "-- maxCMI:", round(maxCMI, 2), "-- ratio:", round(ratio, 2), "\n")
    
  } # end for i
  
}