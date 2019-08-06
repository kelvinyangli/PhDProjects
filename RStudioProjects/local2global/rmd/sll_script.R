# if data already exists in .rds format, then convert data to numeric format
# for sll and save in directory
for (nn in c(3200,6400,12800)) {
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
for (nn in c(3200,6400,12800)) {
  dts = list.files(paste0("data_rds_", nn))
  dir.create(paste0("mb_sll_", nn), showWarnings = F)
  for (i in 1:length(dts)) {
    system(paste0("./sll data_sll_", nn, "/", dts[i],
                  " -a sll-mb -t all --output-mb-file mb_sll_", nn, "/", dts[i]))
  }
}
