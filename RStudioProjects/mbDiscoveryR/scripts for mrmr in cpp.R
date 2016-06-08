
#allNodes = names(data)

varname = "V8"
i = which(names(data) == varname)
temp = data[, i]
tempdata = cbind(temp, data[, -i])
names(tempdata)[1] = varname
  
write.csv(tempdata, "data.csv", row.names = FALSE)

system("mrmr_win32 -i data.csv")
