setwd(dirname(parent.frame(2)$ofile))

load("data/brain.rda")

rel_abund = HTSet::HTSet(
    edata = as(data$rel_abund$conc_table, "matrix"),
    pdata = as(data$rel_abund$sample_table, "data.frame"),
    fdata = as(data$rel_abund$feature_data, "data.frame")
)

abs_abund = HTSet::HTSet(
    edata = as(data$abs_abund$conc_table, "matrix"),
    pdata = as(data$abs_abund$sample_table, "data.frame"),
    fdata = as(data$abs_abund$feature_data, "data.frame")
)

data = list(
    rel_abund = rel_abund,
    abs_abund = abs_abund
)

data$rel_abund$pdata$region = gsub(" ", "", data$rel_abund$pdata$region)
data$abs_abund$pdata$region = gsub(" ", "", data$abs_abund$pdata$region)
data$rel_abund$pdata$group = gsub("-", "", data$rel_abund$pdata$group)
data$abs_abund$pdata$group = gsub("-", "", data$abs_abund$pdata$group)

saveRDS(data, file = "data.rds")