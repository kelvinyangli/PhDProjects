libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2")
lapply(libraries, require, character.only = TRUE)

options(scipen = 10) # print numerical values to fixed within 10 digits instead of exponential
options(digits = 3) # print to the 3rd decimal place, default is 7
