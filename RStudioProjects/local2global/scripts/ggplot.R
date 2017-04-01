#require(reshape2)
#library(ggplot2)


models = c("ALARM", "ALARM3", "CHILD", "CHILD3", "BARLEY", "INSURANCE", "INSURANCE3", "INSURANCE5")
#models = c("INSURANCE", "INSURANCE3", "INSURANCE5")
measureMB = TRUE
if (!measureMB) {
  data = read.csv("../../../UAI_exp/plot1.csv")
  data$Method = factor(data$Method, levels = c("MBPT", "SLL+C", "SLL+G", "MMHC", "GES", "CaMML"), 
                       labels = c("MBPT", "SLL+C", "SLL+G", "MMHC", "GES", "CaMML"))  
} else {
  data = read.csv("../../../UAI_exp/plot2.csv")
  data$Method = factor(data$Method, levels = c("MBPT", "SLL", "HITON"), labels = c("MBPT", "SLL", "HITON"))  
}
data$Sample = as.factor(data$Sample)

for (i in 1:length(models)) {
  if (!measureMB) {
    ggplot(data[which(data$Network == models[i]),], aes(x = Sample, y = Edit_distance, fill = Method)) + 
      geom_point(aes(color = Method, shape = Method), size = 3, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = Edit_distance - CI, ymax = Edit_distance + CI),
                    width = 0.1, position = position_dodge(width = 0.5)) +
      labs(title = models[i]) + 
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), legend.key.size = unit(0.3, "in"))
    filename = tolower(models[i])
    ggsave(paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Scaling-up polytree learning/UAI 2017/figures/", 
                  filename, ".pdf"))
  } else {
    if (length(which(data$Network == models[i])) > 0) {
      ggplot(data[which(data$Network == models[i]),], aes(x = Sample, y = False_finding, fill = Method)) + 
        geom_point(aes(color = Method, shape = Method), size = 3, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = False_finding - CI, ymax = False_finding + CI),
                      width = 0.1, position = position_dodge(width = 0.5)) +
        labs(title = models[i]) + 
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), legend.key.size = unit(0.3, "in"))
      filename = paste0(tolower(models[i]), "_mb")
      ggsave(paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Scaling-up polytree learning/UAI 2017/figures/", 
                    filename, ".pdf"))
      
    }
    
  }
  
  
}


