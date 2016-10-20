# speed test

setwd("realWorldModelWithTrueParameters")
model = "alarm"
cpts = read.dsc(paste0(model, "/cpts/", model, ".dsc"))
allNodes = names(cpts)

n = 500

# mmlcpt
datasets = list.files(paste0(model, "/data rds/"), pattern = paste0("_", n, "_"))
ii = 17
data = readRDS(paste0(model, "/data rds/", datasets[ii]))


dataInfo = getDataInfo(data) 
mbList = list()

# compute mb of each node using standard forward selection
system.time(
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  mbList[[i]] = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
  
} # end for i 
)


# pcmb
datasets = list.files(paste0(model, "/data/"), pattern = paste0("_", n, "_"))

setwd("pcmb2/") # set wd to pcmb folder

file.copy(paste0("../", model, "/data/", datasets[ii]), paste0(model, ".data"), overwrite = TRUE) # copy data from "alarm data" to "pcmb" with new name "alarm.data"
  
system.time(system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 1 0.01"), intern = TRUE))

file.remove("output.txt")
  
# iamb
system.time(system(paste0("kmb4 ", model, ".data ", n, " ", length(allNodes), " -1 1.0 1 0 0.01"), intern = TRUE))
  
file.remove("output.txt")
  
setwd("..")
