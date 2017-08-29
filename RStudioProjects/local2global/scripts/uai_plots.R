#require(reshape2)
#library(ggplot2)

models = c("ALARM", "ALARM3", "ALARM5", "CHILD", "CHILD3", "CHILD5", "INSURANCE", "INSURANCE3", "INSURANCE5", "BARLEY", "HAILFINDER")
#models = c("INSURANCE", "INSURANCE3", "INSURANCE5")
measureMB = TRUE
if (!measureMB) {
  data = read.csv("~/Documents/Experiments/UAI_exp/UAI_exp/plot1.csv")
  data$Method = factor(data$Method, levels = c("MBMML", "SLL+C", "SLL+G", "MMHC", "GES", "CaMML"), 
                       labels = c("MBMML", "SLL+C", "SLL+G", "MMHC", "GES", "CaMML"))  
} else {
  data = read.csv("~/Documents/Experiments/UAI_exp/mb_accuracy.csv")
  data$Method = factor(data$Method, levels = c("MBMML", "SLL", "HITON"), labels = c("MBMML", "SLL", "HITON"))  
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
    ggsave(paste0("~/Desktop/Mid-candidature review/figures/", filename, ".pdf"), width = 8, height = 5)
  } else {
    if (length(which(data$Network == models[i])) > 0) {
      ggplot(data[which(data$Network == models[i]),], aes(x = Sample, y = Edit_distance, fill = Method)) + 
        geom_point(aes(color = Method, shape = Method), size = 3, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = Edit_distance - CI, ymax = Edit_distance + CI),
                      width = 0.1, position = position_dodge(width = 0.5)) + 
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), legend.key.size = unit(0.3, "in"))
      filename = paste0(tolower(models[i]), "_mb")
      ggsave(paste0("~/Desktop/Mid-candidature review/figures/", filename, ".pdf"), width = 8.3, height = 4.5)
    }
  }
}


