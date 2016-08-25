# divide the mb discovery process into two phases, where
# the 1st phase finds pc(target), and the 2nd phase finds
# pc(x), for all x \in pc(target), then we could apply an 
# elimination step to filter the false positives of mb(target)

mb2stage = function(data, targetNode, score, dataInfo) {
  
  allNodes = colnames(data)
  
  pcLearned = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
  
  
}