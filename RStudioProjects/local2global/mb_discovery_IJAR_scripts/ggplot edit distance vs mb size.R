setwd("~/Documents/Experiments/mb_discovery_IJAR/")
nvars = 50
model = paste0(nvars, "_5_4_1")
true = list.files(paste0(model, "/mb_true"))
dags = list.files(paste0(model, "/dag"))
mbt_len_mtx = matrix(0, 5, nvars)
for (i in 1:length(dags)) {
  mbt = readRDS(paste0(model, "/mb_true/", true[i]))
  mbt_len_mtx[i, ] = sapply(mbt, length)
}

for (n in c(100, 500, 2000, 5000)) {

  #n = 2000
  mbt_len_unique = unique(as.vector(mbt_len_mtx))
  m = matrix(0, length(mbt_len_unique), 7)
  m[, 1] = mbt_len_unique[order(mbt_len_unique)]
  w = m

  for (ii in 1:6) {
    folders = c("mml_cpt", "mml_nb", "mml_rand", "iamb_bnlearn_0.01", "pcmb_0.01", "sll")
    folder = folders[ii]
    ll = rep(list(c()), nrow(m))
    learned = list.files(paste0(model, "/", folder), pattern = paste0("_", n, "_"))
    for (i in 1:5) {
      dag = readRDS(paste0(model, "/dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mbt = readRDS(paste0(model, "/mb_true/", true[i]))
      #edmtx = matrix(0, 5, length(mbt)) # edit distance
      #jj = 1
      for (j in ((5 * (i - 1)) + 1):(5 * i)) {
        mbl = readRDS(paste0(model, "/", folder, "/", learned[j]))
        if (folder %in% c("mml_cpt", "mml_rand")) {
          mbl = symmetry_correction(vars, mbl, "union")
        } else if (folder == "mml_nb") {
          mbl = symmetry_correction(vars, mbl, "intersection")
        }
        for (k in 1:length(mbt)) {
          ind = which(m[,1] == length(mbt[[k]]))
          ll[[ind]] = c(ll[[ind]], mb_false_finding(mbt[[k]], mbl[[k]]))
          # m[ind, ii + 1] = m[ind, ii + 1] + mb_false_finding(mbt[[k]], mbl[[k]])
          # w[ind, ii + 1] = w[ind, ii + 1] + 1
        }
        #jj = jj + 1
      }
      #write.table(edmtx, "~/Documents/Experiments/kdd_exp/kdd_results.csv", sep = ",", append = T, col.names = F, row.names = F)
      #ll[[i]] = edmtx
    }
    m[, ii + 1] = sapply(ll, mean)
    w[, ii + 1] = 1.96 * sapply(ll, sd) / sqrt(sapply(ll, length))

  }

  m = round(m, 1)
  w = round(w, 1)
  colnames(m) = colnames(w) = c("nmb", "MML_CPT", "MML_NB", "MBMML_MBP", "IAMB", "PCMB", "SLL")
  m = m[, c(1:4, 7, 5, 6)]
  m = data.frame(m)
  m_melt = melt(m, id = "nmb")
  colnames(m_melt)[2] = "Algorithm"
  error = as.vector(w[, -1])
  figure = ggplot(m_melt, aes(x = nmb, y = value, group = Algorithm, colour = Algorithm, linetype = Algorithm)) +
    ylab(label = "Edit distance") + xlab("Markov blanket size") + geom_line(aes(linetype = Algorithm)) + geom_point(aes(shape = Algorithm)) + scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
    ylim(0, ceiling(max(m_melt$value+error))) + xlim(0, max(mbt_len_mtx)) + guides(linetype = guide_legend()) +
    theme(legend.key.width = unit(1.5, "cm")) +
    geom_errorbar(aes(ymin = value - error, ymax = value + error), width = 0.03)
  figure
  filename = paste0("ed_vs_mbsize_", nvars, "_5_4_1_", n, collapse = "")
  filename = paste0(filename, ".pdf", collapse = "")
  ggsave(filename, device = "pdf", path = "ggplot", width = 7.29, height = 4.5, units = "in")

}
