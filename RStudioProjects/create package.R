# creating an R package 
install.packages("devtools")
library(devtools)
devtools::install_github("klutometis/roxygen") # install the developer version of the roxygen2 package 
library(roxygen2)

# 1. create your package directory 
create("causalbn")
# 2. add functions
# 3. add documentation
# 4. process your documentation
document()
# 5. install
setwd("../")
install("causalbn/")
# 6. make the package a github repo
install_github("causalbn", "kelvinyangli/causalbn", subrdir = "PhDProjects/RStudioProjects")
# check packages info 
devtools::session_info()
