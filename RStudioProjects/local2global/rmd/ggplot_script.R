library(ggplot2)
library(reshape2)
library(gridExtra)
#3200 
mml = c(144.19,140.27,135.16,127.5,119.76,109.08,99.66,83.92,68.13,51.52,32.95)
iamb = c(175.5,170.95,165.97,156.84,151.28,140.21,128.37,118.18,102.5,85.68,69.39)
pcmb = c(154.54,152.55,149.57,146.37,141.8,137.55,132.75,124.14,118.62,109.39,100.14)
error = c(3.1,3.15,3.3,3.1,3.06,2.97,2.86,2.71,2.54,2.14,1.15,1.36,
          3.39,3.36,3.36,3.42,3.48,3.08,3.1,3.38,2.77,2.45,1.75,1.21,
          2.6,2.53,2.65,2.55,2.49,2.38,2.46,2.41,2.29,2.09,1.89,1.95)
m = data.frame(p=seq(0,1,0.1),mml,iamb,pcmb)
m = rbind(m, c(1.1, 68.48,65.22,110.17))
m_melt = melt(m, id = "p")
colnames(m_melt)[2] = "Algorithm"
figure1 = ggplot(m_melt, aes(x = p, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("Percentage hold fixed") + geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  guides(linetype = guide_legend()) + theme(legend.key.width = unit(1.5, "cm")) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) +
  scale_x_continuous(breaks = seq(0, 1.1, 0.1), labels = c(seq(0,1,0.1),"w/o")) +
  scale_y_continuous(breaks = seq(floor(min(m_melt$value-error)), ceiling(max(m_melt$value+error)), 20))

figure1
# filename = paste0("ed_vs_samplesize_", model, ".pdf", collapse = "")
# ggsave(filename, device = "pdf", path = "ggplot", width = 7.29, height = 4.5, units = "in")

#6400 
mml = c(144.06,137.52,133.72,123.85,117.67,106.49,95.56,83.22,63.76,44.32,25.92)
iamb = c(174.43,171.37,164.02,159.31,150.15,143.22,134.67,119.61,101.65,85.32,67.84)
pcmb = c(156.47,153.38,149.25,145.09,139.63,134.16,126.79,118.31,108.53,96.98,85.73)
error = c(3.46,3.16,3.53,3.45,3.27,3.21,3.23,2.91,2.73,2.07,1.05,1.23,
          3.5,3.82,3.58,3.47,3.51,3.5,3.58,3.43,3.2,2.61,1.83,1.01,
          3.07,3.07,2.82,2.96,2.83,2.77,2.69,2.62,2.49,2.31,1.8,1.74)
m = data.frame(p=seq(0,1,0.1),mml,iamb,pcmb)
m = rbind(m, c(1.1, 55.45,52.36,90.45))
m_melt = melt(m, id = "p")
colnames(m_melt)[2] = "Algorithm"
figure2 = ggplot(m_melt, aes(x = p, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("Percentage hold fixed") + geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  guides(linetype = guide_legend()) + theme(legend.key.width = unit(1.5, "cm")) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) +
  scale_x_continuous(breaks = seq(0, 1.1, 0.1), labels = c(seq(0,1,0.1),"w/o")) +
  scale_y_continuous(breaks = seq(floor(min(m_melt$value-error)), ceiling(max(m_melt$value+error)), 20))

figure2
# filename = paste0("ed_vs_samplesize_", model, ".pdf", collapse = "")
# ggsave(filename, device = "pdf", path = "ggplot", width = 7.29, height = 4.5, units = "in")

#12800
mml = c(143.06,137.13,130.24,123.12,117.75,102.63,89.77,78.43,65.28,41.85,21.32)
iamb = c(175.57,171.13,163.51,157.26,150.62,140.26,133.17,120.39,104.97,84.55,66.55)
pcmb = c(160.9,159.63,156.34,151.45,140.81,134.97,125.83,117.03,105.06,91.28,77.36)
error = c(3.81,3.47,3.51,3.5,3.32,3.31,3.26,2.93,2.97,2.23,0.93,1.08,
          3.96,3.67,3.52,3.56,3.65,3.3,4.06,3.56,3.47,2.79,1.99,0.87,
          3.39,3.47,3.4,3.31,3.42,3.3,3.09,3.08,2.86,2.56,1.86,1.64)
m = data.frame(p=seq(0,1,0.1),mml,iamb,pcmb)
m = rbind(m, c(1.1, 44.11,42.15,73.14))
m_melt = melt(m, id = "p")
colnames(m_melt)[2] = "Algorithm"
figure3 = ggplot(m_melt, aes(x = p, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("Percentage hold fixed") + geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  guides(linetype = guide_legend()) + theme(legend.key.width = unit(1.5, "cm")) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) +
  scale_x_continuous(breaks = seq(0, 1.1, 0.1), labels = c(seq(0,1,0.1),"w/o")) +
  scale_y_continuous(breaks = seq(floor(min(m_melt$value-error)), ceiling(max(m_melt$value+error)), 20))

figure3
# filename = paste0("ed_vs_samplesize_", model, ".pdf", collapse = "")
# ggsave(filename, device = "pdf", path = "ggplot", width = 7.29, height = 4.5, units = "in")

grid.arrange(figure1,figure2,figure3,ncol=3)


