# nFeatures is the number of features to be returned
# it needs to be pre-determined for mrmr
# allNodes depends on each dataset that is feed to mrmr, since we need to move each column to the 1st position
parseMRMR = function(output, nFeatures) {
  
  #features = allNodes[-1] # remove the 1st variable since it is the target
  startString = "*** mRMR features ***" # allocate the results of mrmr from entire printing results
  indexStart = pmatch(startString, output) + 2 # pass the next index which contains parameter settings for mrmr
  
  mrmrOutput = output[indexStart:(indexStart + nFeatures - 1)]
  mb = vector(length = nFeatures)
  
  for (i in 1:nFeatures) {# for each feature
    
    string = mrmrOutput[i]
    mb[i] = strsplit(string, "\"")[[1]][2]
    
  } # end for i
  
  return(mb)
  
}