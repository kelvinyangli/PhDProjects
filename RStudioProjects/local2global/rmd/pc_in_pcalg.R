pc_algorithm = function(data, alpha) {

  allNodes = colnames(data)

  nlev = rep(2, ncol(data)) # set initial arity to 2

  for (j in 1:ncol(data)) nlev[j] = nlevels(data[,j]) # get arities for nodes from data

  # convert data into 0, 1, 3, ...
  dataConverted = matrix(0, nrow = nrow(data), ncol = ncol(data))

  for (k in 1:ncol(data)) dataConverted[,k] = as.numeric(data[,k]) - 1

  colnames(dataConverted) = allNodes

  suffStat = list(dm = dataConverted, nlev = nlev, adaptDF = FALSE)

  dagPC = pcalg::pc(suffStat, indepTest = disCItest, alpha = alpha, labels = allNodes)

  return(dagPC)

}


# convert cpdag from class pcalg to a dag in bn
# the cpdag to dag extension is done using pcalg:pdag2dag or bnlearn:cextend
# the algorithm is in D.Dor, M.Tarsi (1992). A simple algorithm to construct
# a consistent extension of a partially oriented graph.

pcalg2bnlearn = function(cpdagPC) {

  dagPC = pcalg::pdag2dag(cpdagPC@graph)$graph # extend a pdag to dag using pcalg::pdag2dag

  # convert dag from class pcalg to bn
  allNodes = dagPC@nodes

  dagMatrix = matrix(0, nrow = length(allNodes), ncol = length(allNodes), dimnames = list(allNodes, allNodes))

  for (i in 1:nrow(dagMatrix)) dagMatrix[allNodes[i], dagPC@edgeL[[i]]$edges] = 1

  dagBN = empty.graph(allNodes)

  for (i in 1:nrow(dagMatrix)) {

    # adjacent nodes to allnodes[i]
    adjacentNodes = allNodes[dagMatrix[i,] == 1]

    if (length(adjacentNodes) > 0) {

      for (j in 1:length(adjacentNodes)) dagBN = set.arc(dagBN, allNodes[i], adjacentNodes[j])

    } # end if

  } # end for i

  return(dagBN)

}

