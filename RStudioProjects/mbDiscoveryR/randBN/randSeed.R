# this function generate random seed based on the local computer's lower digit
randSeed = function() {
  
  op = options(digits.secs = 6)
  x = gsub("[: -]", "" , Sys.time(), perl = TRUE) # remove - and : 
  x = strsplit(x, split = "[.]")[[1]][2] # get lower digits
  x = as.numeric(x) # convert char to numeric 
  return(x)
  
}