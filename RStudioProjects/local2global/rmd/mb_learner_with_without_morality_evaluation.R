################################################################################
# evaluation on synthetic node orders
################################################################################
dags = list.files("dag/", pattern = ".rds")
ed_mtx = pre_mtx = rec_mtx = matrix(0, ncol = 12, nrow = 400*6)
alg = "pcmb"
col_cnt = 1
for (p in seq(0, 1, 0.1)) {
  row_cnt = 1
  for (nn in c(400,800,1600,3200,6400,12800)) { # no morality
    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      files = list.files(paste0("mb_", alg, "_", nn), pattern = name)
      dag = readRDS(paste0("dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mr = dag2matrix(bnlearn::moral(dag))
      for (j in 1:length(files)) {
        ord2 = rev(readRDS(paste0("node_orders_random/", name, "_", p, ".rds")))
        if (alg == "sll") {
          mbcpt = sll2list(paste0("mb_", alg, "_", nn, "/", files[j]), vars)
        } else {
          mbcpt = readRDS(paste0("mb_", alg, "_", nn, "/", files[j]))
        }
        if (alg == "pcmb") {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
        } else if (alg == "sll") {
          mbcpt_sym = mbcpt
        } else {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
        }
        G = mblist2moral(mbcpt_sym, vars)
        # res = edit_dist_graph(G, mr)
        # ed_mtx[row_cnt, 12] = res$ed
        # pre_mtx[row_cnt, 12] = res$pre
        # rec_mtx[row_cnt, 12] = res$rec

        # G1 = wrsgraph::fixed_ordering_moralization(G, ord2)
        G1 = wrsgraph::moralization(G, ord2, 0)
        res = edit_dist_graph(G1, mr)
        ed_mtx[row_cnt, col_cnt] = res$ed
        pre_mtx[row_cnt, col_cnt] = res$pre
        rec_mtx[row_cnt, col_cnt] = res$rec

        row_cnt = row_cnt + 1
      }

    }

  }
  col_cnt = col_cnt + 1
}


write.xlsx(ed_mtx, paste0("random_node_order_moralization_", alg, ".xlsx"), sheetName="ed", col.names=TRUE, row.names=F, append=T)
write.xlsx(pre_mtx, paste0("random_node_order_moralization_", alg, ".xlsx"), sheetName="pre", col.names=TRUE, row.names=F, append=T)
write.xlsx(rec_mtx, paste0("random_node_order_moralization_", alg, ".xlsx"), sheetName="rec", col.names=TRUE, row.names=F, append=T)

################################################################################
# evaluation on learned node orders
################################################################################
dags = list.files("dag/", pattern = ".rds")
ed_mtx = pre_mtx = rec_mtx = matrix(0, ncol = 5, nrow = 400*6)
for (col_cnt in 1:4) {
  row_cnt = 1
  for (nn in c(400,800,1600,3200,6400,12800)) { # no morality
    for (i in 1:length(dags)) {
      dag = readRDS(paste0("dag/", dags[i]))
      root = node.ordering(dag)[1]
      vars = nodes(dag)
      mr = dag2matrix(moral(dag))
      name = strsplit(dags[i], ".rds")[[1]][1]
      files = list.files(paste0("mb_mmlcpt_", nn), pattern = name)

      for (j in 1:length(files)) {

        mbl = readRDS(paste0("mb_mmlcpt_", nn, "/", files[j]))
        mbl = symmetry_correction(vars, mbl, "union")
        g_mbl = mblist2moral(mbl, vars)

        if (col_cnt == 1) {
          g = min_deg_moralization(g_mbl)
        } else if (col_cnt == 2) {
          g = min_deficiency_moralization(g_mbl)
        } else if (col_cnt == 3) {
          ord = node.ordering(readRDS(paste0("mmhc_", nn, "/", files[j])))
          ord = rev(ord)
          g = moralization(g_mbl, ord, 1)
        } else if (col_cnt == 4) {
          tree = readRDS(paste0("chow_liu_", nn, "/", files[j]))
          ord = node.ordering(directing_tree(tree, root))
          ord = rev(ord)
          g = moralization(g_mbl, ord, 1)
        }

        res = edit_dist_graph(g, mr)
        ed_mtx[row_cnt, col_cnt] = res$ed
        pre_mtx[row_cnt, col_cnt] = res$pre
        rec_mtx[row_cnt, col_cnt] = res$rec

        row_cnt = row_cnt + 1
      }

    }
  }

}

write.xlsx(ed_mtx, paste0("learned_node_order_moralization.xlsx"), sheetName="ed", col.names=TRUE, row.names=F, append=T)
write.xlsx(pre_mtx, paste0("learned_node_order_moralization.xlsx"), sheetName="pre", col.names=TRUE, row.names=F, append=T)
write.xlsx(rec_mtx, paste0("learned_node_order_moralization.xlsx"), sheetName="rec", col.names=TRUE, row.names=F, append=T)


################################################################################
# evaluation on different approximated node ordering, eg mmhc, min deg, min def
################################################################################
dags = list.files("dag/", pattern = ".rds")
for (alg in c("mmlcpt","iamb","pcmb")) {
  for (nn in c(400,800,1600)) { # no morality

    mbed = pre = rec = c()
    mbed1 = pre1 = rec1 = c() # mini deg
    mbed2 = pre2 = rec2 = c() # mini deficiency
    mbed3 = pre3 = rec3 = c() # mmhc node ordering
    mbed4 = pre4 = rec4 = c() # a true ordering from the true dag
    mbed5 = pre5 = rec5 = c() # mmhc moral graph accuracy
    mbed6 = pre6 = rec6 = c() # mmhc node ordering, triangulation
    mbed7 = pre7 = rec7 = c() # a true ordering from the true dag, triangulation

    for (i in 1:length(dags)) {
      name = strsplit(dags[i], ".rds")[[1]]
      # mmhc learnd dag and mb_mmlcpt and data have the same name
      files = list.files(paste0("mb_", alg, "_", nn), pattern = name)
      if (length(files) == 0) break
      dag = readRDS(paste0("dag/", dags[i]))
      vars = bnlearn::nodes(dag)
      mr = dag2matrix(bnlearn::moral(dag))
      ord = rev(bnlearn::node.ordering(dag))
      for (j in 1:length(files)) {
        dagMMHC = readRDS(paste0("mmhc_", nn, "/", files[j]))
        ord2 = rev(node.ordering(dagMMHC))
        if (alg == "sll") {
          mbcpt = sll2list(paste0("mb_", alg, "_", nn, "/", files[j]), vars)
        } else {
          mbcpt = readRDS(paste0("mb_", alg, "_", nn, "/", files[j]))
        }

        if (alg == "pcmb") {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "intersection")
        } else if (alg == "sll") {
          mbcpt_sym = mbcpt
        } else {
          mbcpt_sym = symmetry_correction(vars, mbcpt, "union")
        }
        G = mblist2moral(mbcpt_sym, vars)
        # enforce morality
        G1 = wrsgraph::min_deg_moralization(G)
        G2 = wrsgraph::min_deficiency_moralization(G)
        G3 = wrsgraph::fixed_ordering_moralization(G, ord2)
        G4 = wrsgraph::fixed_ordering_moralization(G, ord)
        G5 = dag2matrix(moral(dagMMHC))
        G6 = moralization(G, ord2, 0)
        G7 = moralization(G, ord, 0)

        # k = k + 1
        res = edit_dist_graph(G, mr)
        mbed = c(mbed, res$ed)
        pre = c(pre, res$pre)
        rec = c(rec, res$rec)

        res1 = edit_dist_graph(G1, mr)
        mbed1 = c(mbed1, res1$ed)
        pre1 = c(pre1, res1$pre)
        rec1 = c(rec1, res1$rec)

        res2 = edit_dist_graph(G2, mr)
        mbed2 = c(mbed2, res2$ed)
        pre2 = c(pre2, res2$pre)
        rec2 = c(rec2, res2$rec)

        res3 = edit_dist_graph(G3, mr)
        mbed3 = c(mbed3, res3$ed)
        pre3 = c(pre3, res3$pre)
        rec3 = c(rec3, res3$rec)

        res4 = edit_dist_graph(G4, mr)
        mbed4 = c(mbed4, res4$ed)
        pre4 = c(pre4, res4$pre)
        rec4 = c(rec4, res4$rec)

        res5 = edit_dist_graph(G5, mr)
        mbed5 = c(mbed5, res5$ed)
        pre5 = c(pre5, res5$pre)
        rec5 = c(rec5, res5$rec)

        res6 = edit_dist_graph(G6, mr)
        mbed6 = c(mbed6, res6$ed)
        pre6 = c(pre6, res6$pre)
        rec6 = c(rec6, res6$rec)

        res7 = edit_dist_graph(G7, mr)
        mbed7 = c(mbed7, res7$ed)
        pre7 = c(pre7, res7$pre)
        rec7 = c(rec7, res7$rec)
      }
    }

    write.table(paste0(nvars, "_", maxNPas, "_2_1_", nn, "_", p), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)
    # no moral
    x = round(c(mean(mbed), 1.96*sd(mbed)/sqrt(length(mbed)), mean(pre), mean(rec)), 2)
    write.table(matrix(x, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # mini deg
    x1 = round(c(mean(mbed1), 1.96*sd(mbed1)/sqrt(length(mbed1)), mean(pre1), mean(rec1)), 2)
    write.table(matrix(x1, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # mini deficiency
    x2 = round(c(mean(mbed2), 1.96*sd(mbed2)/sqrt(length(mbed2)), mean(pre2), mean(rec2)), 2)
    write.table(matrix(x2, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # mmhc ordering, moralization
    x3 = round(c(mean(mbed3), 1.96*sd(mbed3)/sqrt(length(mbed3)), mean(pre3), mean(rec3)), 2)
    write.table(matrix(x3, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # mmhc ordering, triangulation
    x6 = round(c(mean(mbed6), 1.96*sd(mbed6)/sqrt(length(mbed6)), mean(pre6), mean(rec6)), 2)
    write.table(matrix(x6, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # mmhc moral graph
    x5 = round(c(mean(mbed5), 1.96*sd(mbed5)/sqrt(length(mbed5)), mean(pre5), mean(rec5)), 2)
    write.table(matrix(x5, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # true ordering, moralization
    x4 = round(c(mean(mbed4), 1.96*sd(mbed4)/sqrt(length(mbed4)), mean(pre4), mean(rec4)), 2)
    write.table(matrix(x4, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    # true ordering, triangulation
    x7 = round(c(mean(mbed7), 1.96*sd(mbed7)/sqrt(length(mbed7)), mean(pre7), mean(rec7)), 2)
    write.table(matrix(x7, nrow = 1), paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

    write.table("***", paste0("results_", alg, ".txt"), append = T, row.names = F, col.names = F)

  }
}












