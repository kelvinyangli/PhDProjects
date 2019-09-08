# if data already exists in .rds format, then convert data to numeric format
# for sll and save in directory
for (nn in c(400,800,1600)) {
  dts = list.files(paste0("data_rds_", nn))
  dir.create(paste0("data_sll_", nn), showWarnings = F)
  for (i in 1:length(dts)) {
    data = readRDS(paste0("data_rds_", nn, "/", dts[i]))
    data = factor2numeric(data)
    name = strsplit(dts[i], ".rds")[[1]][1]
    write.table(data, paste0("data_sll_", nn, "/", name), row.names = F, col.names = F)
  }
}


# learn bn using sll
for (nn in c(1600)) {
  dts = list.files(paste0("data_sll_", nn))
  dir.create(paste0("sll_", nn), showWarnings = F)
  for (i in 1:length(dts)) {
    system(paste0("./sll data_sll_", nn, "/", dts[i],
                  " -a sll+g -t all --output-dag-file sll_", nn, "/", dts[i]))
  }
}


