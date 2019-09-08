library(ggplot2)
library(reshape)

################################################################################
# calculate the kt distance of the orders learned by mmhc 
################################################################################
dags = list.files("dag/", pattern = ".rds")
m = c()
for (nn in c(400,800,1600,3200,6400,12800)) {
  for (i in 1:length(dags)) {
    name = strsplit(dags[i], ".rds")[[1]]
    files = list.files(paste0("mmhc_", nn, "/"), pattern = name)
    dag = readRDS(paste0("dag/", dags[i]))
    ord_t = rev(node.ordering(dag))
    for (j in 1:length(files)) {
      dag_mmhc = readRDS(paste0("mmhc_", nn, "/", files[j]))
      ord_l = rev(node.ordering(dag_mmhc))
      m = c(m, kendall_tau_distance(ord_t, ord_l))
    }
  }
}

m = matrix(m, ncol = 6, byrow = F)
colMeans(m)
# normalized KT distance 
colMeans(m) / (49*25)

################################################################################
# calculate the kt distance of the random orders
################################################################################
ords = list.files("node_orders_random//")
ktd = c()
for (i in 1:40) {
  ord_t = rev(readRDS(paste0("node_orders_random/", ords[i*11])))
  ord_r = rev(readRDS(paste0("node_orders_random/", ords[i*11-1])))
  ktd = c(ktd, kendall_tau_distance(ord_t, ord_r))
  for (j in ((i*11-10):(i*11-2))) {
    ord_r = rev(readRDS(paste0("node_orders_random/", ords[j])))
    ktd = c(ktd, kendall_tau_distance(ord_t, ord_r))
  }
  ktd = c(ktd, 0)
}

# store in a matrix
m = matrix(ktd, ncol = 11, byrow = T)
# percentage of pairwise agreements
xaxisLables = round(1- colMeans(m) / (50*49/2), 2)

xaxisLables = seq(0, 1, 0.1) # when using synthetic orders

################################################################################
# plot, synthetic orders
################################################################################
m = read.csv("ed_mean_synthetic_orders_triangulation_xlabel_ktd.csv")
error = read.csv("ed_ci_synthetic_orders_triangulation.csv")
error = error$ci
m_melt = melt(m, id = c("p", "n"))
names(m_melt)[3] = "Algorithm"
m_melt$n = factor(m_melt$n, levels = c("n=400","n=800","n=1600","n=3200", "n=6400", "n=12800"))
figure = ggplot(m_melt, aes(x = p, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("Percentage of pairwise agreement") + geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  guides(linetype = guide_legend()) + theme(legend.key.width = unit(1.5, "cm")) +
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) +
  scale_x_continuous(breaks = c(xaxisLables, 1.05), labels = c(xaxisLables,"w/o")) +
  scale_y_continuous(breaks = seq(40, 320, 40))

figure + facet_wrap(.~n, ncol = 2) + theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggsave("random_node_orders_morality.pdf", device = "pdf", width = 7.29, height = 4.5, units = "in")

# a4 paper size 
# synthetic_orders_moralization_xlabel_ktd.pdf
# synthetic_orders_moralization_xlabel_hold_fixed_percentage.pdf
# synthetic_orders_triangulation_xlabel_ktd.pdf
ggsave("synthetic_orders_triangulation_xlabel_ktd.pdf", device = "pdf", width = 8.27, height = 11.69, units = "in")

################################################################################
# plot, learned orders
################################################################################
m = read.csv("ed_mean_learned_orders_moralization.csv")
names(m)[7] = "SLL+G"
error = read.csv("ed_ci_learned_orders_moralization.csv")
error = error$ci
m_melt = melt(m, id = c("n"))
names(m_melt)[2] = "Algorithm"
# m_melt$n = factor(m_melt$n, levels = c("n=100*log4","n=100*log8","n=100*log16","n=3200", "n=6400", "n=12800"))
figure = ggplot(m_melt, aes(x = n, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
  ylab(label = "Edit distance") + xlab("log2(n/100)") + geom_line(aes(linetype = Algorithm)) +
  geom_point(aes(shape = Algorithm)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "red")) +
  guides(linetype = guide_legend()) + theme(legend.key.width = unit(1.5, "cm")) +
  scale_y_continuous(breaks = seq(0,200,20)) + 
  geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03) 

figure
ggsave("learned_orders_moralization.pdf", device = "pdf", width = 7.29, height = 4.5, units = "in")

# a4 paper size 
# ggsave("synthetic_orders_triangulation_xlabel_ktd.pdf", device = "pdf", width = 8.27, height = 11.69, units = "in")





