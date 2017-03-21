##########################################################################################
# convert data categories from strings to numebers for matlab 
dir = "../../../../kelvinli/Documents/MATLAB/"
data_name = paste(nVars, maxNPas, maxArity, beta, n, sep = "_", collapse = "")
data_numeric = string2numeric(data)
write.csv(data_numeric, paste0(dir, data_name, ".csv"), row.names = FALSE)

# read learned pt into R from matlab output 
chow = read.csv("testpt.csv")
row.names(chow) = colnames(chow)
chow_matlab = matrix2dag(chow)
#graphviz.plot(matrix2dag(chow))
##########################################################################################
