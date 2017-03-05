#' Generate random seed
#' 
#' This function randomly generate a see from the lower digit on the local machine's clock. The function requires 
#' input parameters. 
#' @examples 
#' randSeed()

randSeed = function() {
  
  op = options(digits.secs = 6)
  x = gsub("[: -]", "" , Sys.time(), perl = TRUE) # remove - and : 
  x = strsplit(x, split = "[.]")[[1]][2] # get lower digits
  x = as.numeric(x) # convert char to numeric 
  return(x)
  
}