# this function reads camml output .dne file into R as text file 
# then parse the camml output to get a parent list for all nodes
netica2bnlearn = function(directory) {
  
  text = read_file(directory)
  bnInfo = strsplit(text, "node")[[1]][-1]
  allNodes = vector(length = length(bnInfo))
  parentsList = list()
  
  for (i in 1:length(bnInfo)) {
    
    info = strsplit(bnInfo[i], "\n\t")[[1]]
    allNodes[i] = trimws(gsub("[[:punct:]]", "", info[1]))
    parents = trimws(gsub("[[:punct:]]", "", strsplit(info[6], "=")[[1]][2]))
    
    if (nchar(parents) > 0) {# if there is at least one parent
      
      parentsList[[i]] = strsplit(parents, " ")[[1]]
      
    } else {# if there is no parent
      
      parentsList[[i]] = vector()
      
    } # end else
    
  } # end for i
  
  names(parentsList) = allNodes
  
  ls = list(allNodes, parentsList)
  names(ls) = c("node", "parent")
  
  return(ls)
  
}

