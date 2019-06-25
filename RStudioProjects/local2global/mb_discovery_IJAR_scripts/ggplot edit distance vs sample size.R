for (model in c("30_5_4_1", "50_5_4_1", "80_5_4_1", "child", "insurance", "alarm", "hailfinder", "barley")) {
# model = "child"
if (model == "30_5_4_1") {
  # 30_5_4_1
  x = c(7.5,7.3,7.2,7.2,7.3,7.4,4.5,6.2,4.6,5.5,5.8,6.3,3.3,5.1,3.5,4.2,5.4,4.4,2.6,4.5,3,3.6,2.7,2.9)
  error = c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.2,0.3,0.2,0.3,0.2,0.3,0.2,0.2,0.2,0.3,0.2,0.2)
} else if (model == "50_5_4_1") {
  # 50_5_4_1
  x = c(10.6,9.7,10.4,9.2,9.5,9.3,6.4,7.9,6.5,7.5,7.6,8,5.1,6.8,5.2,6.1,5.8,6.2,4.2,6,4.5,5.4,4.6,4.4)
  error = c(0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.3,0.2,0.3,0.2,0.3,0.2,0.3,0.2,0.2,0.2,0.3,0.2,0.2)
} else if (model == "80_5_4_1") {
  # 80_5_4_1
  x = c(12,11.4,NA,9.8,10.6,9.8,7.2,8.5,NA,8.1,8.1,8.5,5.6,7,NA,6.7,6.2,6.3,4.8,6.2,NA,5.9,4.7,6.1)
  error = c(0.3,0.3,NA,0.3,0.3,0.3,0.2,0.3,NA,0.3,0.3,0.3,0.2,0.3,NA,0.3,0.2,0.3,0.2,0.2,NA,0.3,0.2,0.3)
} else if (model == "child") {
  # child
  x = c(0.9, 0.9, 1.3, 1.2, 1.3, 1, 0.7, 0.7, 0.9, 1.1, 1, 0.8, 0.5, 0.6, 0.2, 0.7, 0.1, 0.2)
  error = c(0.2, 0.2, 0.2,0.2,0.2,0.2, 0.1,0.2,0.1,0.2,0.2,0.1, 0.1,0.1,0.1,0.2,0,0.1)
} else if (model == "insurance") {
  # insurance
  x = c(3.3,3.5,4,3.4,3.2,3.1,2.9,3.3,3.5,3.1,2.9,2.7,2.1,2.8,2.4,2.7,1.8,2)
  error = c(0.2,0.2,0.3,0.3,0.2,0.2,0.2,0.2,0.3,0.3,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2)
} else if (model == "alarm") {
  # alarm
  x = c(1.4,2.1,3.4,1.9,1.5,0.8,1,1.8,2.8,1.6,1.1,0.6,0.5,1.5,1.7,1.3,0.3,0.2)
  error = c(.1,.2,.2,.2,.1,.1,.1,.2,.2,.2,.1,.1,.1,.2,.1,.2,.1,0)
} else if (model == "barley") {
  # barley
  x = c(4,4.1,4.4,4.3,9,4.2,3.7,3.8,4.2,4.1,NA,3.8,3.4,3.6,3.5,3.8,NA,3.1)
  error = c(.3,.3,.3,.3,.5,.2,.3,.3,.3,.3,NA,.2,.3,.3,.3,.3,NA,.2)
} else if (model == "hailfinder") {
  # hailfinder
  x = c(4.4,4.3,5.2,4.1,7.1,4.3,4.4,4.3,5,4.1,6.2,4.1,4.3,4.3,5.1,4.2,3.8,4)
  error = c(.3,.2,.3,.2,.5,.3,.3,.2,.3,.2,.4,.3,.3,.2,.3,.2,.2,.3)
}

if (model %in% c("30_5_4_1", "50_5_4_1", "80_5_4_1")) {
  m = matrix(x, 4, 6, byrow = T)
  m = cbind((c(100, 500, 2000, 5000)), m)
  colnames(m) = c("nmb", "MBMML_CPT", "MBMML_NB", "MBMML_MBP", "IAMB", "PCMB", "SLL")
  m = m[,c(1:4,7,5,6)]
} else {
  m = matrix(x, 3, 6, byrow = T)
  m = cbind((c(500, 1000, 5000)), m)
  colnames(m) = c("nmb", "MBMML_CPT", "MBMML_NB", "MBMML_MBP", "IAMB", "PCMB", "SLL")
  m = m[,c(1:4,7,5,6)]
}

m = data.frame(m)
m_melt = melt(m, id = "nmb")
colnames(m_melt)[2] = "Algorithm"
figure = ggplot(m_melt, aes(x = nmb, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("Sample size") + geom_line(aes(linetype = Algorithm)) + geom_point(aes(shape = Algorithm)) + scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  ylim(floor(min(m_melt$value-error)), ceiling(max(m_melt$value+error))) + xlim(100, 5000) + guides(linetype = guide_legend()) +
  theme(legend.key.width = unit(1.5, "cm")) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03)
figure
filename = paste0("ed_vs_samplesize_", model, ".pdf", collapse = "")
ggsave(filename, device = "pdf", path = "ggplot", width = 7.29, height = 4.5, units = "in")

}
