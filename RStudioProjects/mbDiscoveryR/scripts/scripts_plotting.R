library(ggplot2)
library(reshape2)

tempMean = matrix(0, 3, 4)
tempMean[1,] = c(0.11,0.3,0.61,0.81)
tempMean[2,] = c(0.11,0.39,0.68,0.82)
tempMean[3,] = c(0.11,0.34,0.62,0.77)


tempMean = as.data.frame(tempMean) # convert matrix to data.frame
colnames(tempMean) = c(100, 500, 1000, 10000)
tempMean = cbind(c(1, 10, 100), tempMean)
colnames(tempMean)[1] = "Prior"
meltTempMean = melt(tempMean, id = "Prior")
meltTempMean[,1] = log10(meltTempMean[,1])
colnames(meltTempMean)[2] = "Samples"
error = c(0.02,0.02,0.02,0.02,0.02,0.02,0.03,0.02,0.02,0.01,0,0.01)

figure = ggplot(meltTempMean, aes(x = Prior, y = value, group = Samples, colour = Samples)) + 
  ylab(label = "F-measure") + xlab("log_10(Prior)") + geom_line(aes(linetype = Samples)) + geom_point(aes(shape = Samples)) + 
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value + error)) + guides(linetype = guide_legend()) +
  guides(linetype = guide_legend())
    #scale_linetype_manual(values = c("solid", "dotted", "longdash")) 

figure = figure + geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) + 
  ylim(min(meltTempMean$value-error), max(meltTempMean$value + error)) + guides(linetype = guide_legend())

#figure = figure + scale_x_continuous(breaks = c(-1, 0, 1, 2))
#figure = figure + labs(title = "34-4-4-1") 
#figure = figure + theme(legend.position="none")
figure = figure + guides(linetype = guide_legend())

figure
