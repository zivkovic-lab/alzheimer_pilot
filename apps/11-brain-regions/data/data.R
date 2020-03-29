setwd(dirname(parent.frame(2)$ofile))

load("../../../data/brain.rda")

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

for(name in names(data)){
    data[[name]]$fdata$subgroups = lapply(seq_len(nfeatures(data[[name]])), function(i){
        subgroup = data[[name]]$fdata$subtypes[i]
        
        if(data[[name]]$fdata$Fuc[i] == 1) subgroup = c(subgroup, "mono-fucosylated")
        else if (data[[name]]$fdata$Fuc[i] == 2) subgroup = c(subgroup, "di-fucosylated")
        else if (data[[name]]$fdata$Fuc[i] == 3) subgroup = c(subgroup, "tri-fucosylated")
        else if (data[[name]]$fdata$Fuc[i] == 4) subgroup = c(subgroup, "tetra-fucosylated")
        
        if(data[[name]]$fdata$NeuAc[i] == 1) subgroup = c(subgroup, "mono-sialylated")
        else if(data[[name]]$fdata$NeuAc[i] == 2) subgroup = c(subgroup, "di-sialylated")
        else if(data[[name]]$fdata$NeuAc[i] == 3) subgroup = c(subgroup, "tri-sialylated")
        else if(data[[name]]$fdata$NeuAc[i] == 4) subgroup = c(subgroup, "tetra-sialylated")
        
        if(data[[name]]$fdata$HexNAc[i] == 4) subgroup = c(subgroup, "bi-antennary")
        else if(data[[name]]$fdata$HexNAc[i] == 5) subgroup = c(subgroup, "tri-antennary")
        else if(data[[name]]$fdata$HexNAc[i] == 6) subgroup = c(subgroup, "tetra-antennary")
        else if(data[[name]]$fdata$HexNAc[i] == 7) subgroup = c(subgroup, "bisecting-glcnac")
        return(subgroup)
    })
}

saveRDS(data, file = "data.rds")