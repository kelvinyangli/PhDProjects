# if data already exists in .rds format, then convert data to numeric format
# for sll and save in directory
# for (nn in c(3200,6400,12800)) {
#   dts = list.files(paste0("data_rds_", nn))
#   dir.create(paste0("data_pcmb_", nn), showWarnings = F)
#   for (i in 1:length(dts)) {
#     data = readRDS(paste0("data_rds_", nn, "/", dts[i]))
#     data = factor2numeric(data)
#     name = strsplit(dts[i], ".rds")[[1]][1]
#     write.csv(data, paste0("data_pcmb_", nn, "/", name, ".csv"), row.names = F)
#   }
# }


for (nn in c(3200,6400,12800)) {
  dts = list.files(paste0("data_pcmb_", nn))
  dir.create(paste0("mb_pcmb_", nn), showWarnings = F)
  for (i in 1:length(dts)) {
    data = read.csv(paste0("data_pcmb_", nn, "/", dts[i]))
    vars = colnames(data)
    name = strsplit(dts[i], ".csv")[[1]][1]
    mb_pcmb = system(paste0("./kmb4_linux data_pcmb_", nn, "/", dts[i], " ", nn, " 50 -1 1.0 0 1 0.01"), intern = T)
    mb_pcmb = pcmb2list(mb_pcmb, vars, "pcmb")
    saveRDS(mb_pcmb, paste0("mb_pcmb_", nn, "/", name, ".rds"))
  }
}
