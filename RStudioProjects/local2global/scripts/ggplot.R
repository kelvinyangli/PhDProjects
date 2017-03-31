#require(reshape2)
#library(ggplot2)

#ed = read.csv("../../../UAI_exp/plot_sample.csv")
#ed$Sample = as.factor(ed$Sample)
str(ed)
pd <- position_dodge(width = 0.2)
ggplot(ed, aes(x = Sample, y = Mean, fill = Method)) + 
  geom_point(aes(color = Method, shape = Method), size = 1.5, position = pd) +
  geom_errorbar(aes(ymin = Mean - CI, ymax = Mean + CI),
                width = 0.1, position = pd) + 
  facet_wrap(~Network)
