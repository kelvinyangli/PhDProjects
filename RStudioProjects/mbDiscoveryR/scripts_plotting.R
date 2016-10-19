

tempMean = matrix(0, 4, 4)
tempMean[1,] = c(0.28,0.5,0.6,0.79)
tempMean[2,] = c(0.35,0.59,0.67,0.81)
tempMean[3,] = c(0.3,0.53,0.6,0.74)
tempMean[4,] = c(0.21,0.39,0.46,0.63)

tempMean = as.data.frame(tempMean) # convert matrix to data.frame
colnames(tempMean) = c(100, 500, 1000, 10000)
tempMean = cbind(c(0.1,1,10,100), tempMean)
colnames(tempMean)[1] = "Prior"
meltTempMean = melt(tempMean, id = "Prior")
meltTempMean[,1] = log10(meltTempMean[,1])
colnames(meltTempMean)[2] = "Samples"
error = c(0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)

figure = ggplot(meltTempMean, aes(x = Prior, y = value, group = Samples, colour = Samples)) + 
  geom_line(data = meltTempMean) + ylab(label = "F-measure") + xlab("log_10(Prior)") + geom_point()

figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value+error))

figure = figure + scale_x_continuous(breaks = c(-1, 0, 1, 2))
figure
