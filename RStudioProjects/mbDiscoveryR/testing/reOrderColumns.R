# this function re order columns for the use of mrmr
# nColumns is the total number of columns in data
# whichColumn refers to which column is going to be put at the 1st position
reOrderColumns = function(nColumns, whichColumn) {
  
  columns = 1:nColumns
  columns[1] = whichColumn 
  columns[whichColumn] = 1
  
  return(columns)
  
}