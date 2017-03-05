# a function to read .dne file into as text and then parse it to get the dag 
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
      
      parentsList[[i]] = c()
      
    } # end else
    
  } # end for i
  
  names(parentsList) = allNodes
  
  ls = list(allNodes, parentsList)
  names(ls) = c("node", "parent")
  
  return(ls)
  
}

