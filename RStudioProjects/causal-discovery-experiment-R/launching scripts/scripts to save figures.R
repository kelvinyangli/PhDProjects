
ggsave(figure, filename = paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Experimental report/figures/log plots/", measure, "_", currentDirectory, ".png"), width = 20, height = 10, unit = "cm")

ggsave(figure, filename = paste0("C:/Users/Administrator/Dropbox/PhD@Monash/Experimental report/figures/log plots/pdf/", measure, "_", currentDirectory, ".pdf"), width = 20, height = 10, unit = "cm")

g1 <- figure

# zoom in to the relevant section of x 
g2 <- figure + coord_cartesian(xlim = c(0, 4), ylim = c(0, 20)) + 
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        legend.position = "none")


# print g1, and then add g2 on top using viewport from package grid
# top right x = 0.65, y = 0.75
# bottom right x = 0.65, y = 0.3
# top left x = 0.2, y = 0.75
g1
print(g2, vp = viewport(x = 0.25, y = 0.75, width = 0.35, height = 0.35))
