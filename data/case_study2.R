setwd(dirname(parent.frame(2)$ofile))

pkgs=c("tidyverse", "tibble", "stringr", "HTSet", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

file = "../raw-data/AD CaseStudy_6antt_20201214.xlsx"

# subject code
ranges = c(
    "Lateral Cerebellar Cortex" = "A2:B18",
    "Medial Temporal Cortex" = "D2:E16",
    "Lateral Pre-Frontal Cortex" = "G2:H20"
)
pdata = lapply(names(ranges), function(region){
    range = ranges[region]
    df = read_excel(file, sheet = "Subject Codes", range = range,
                    col_names = c("group", "sample_id"))
    df$sample_id = toupper(df$sample_id)
    df$subject_id = str_split_fixed(df$sample_id, "_", n = 2)[,1]
    df$region =  sub("\\d", "", str_split_fixed(df$sample_id, "_", n = 2)[,2])
    df$region_fullname = region
    return(df)
}) %>%
    do.call(rbind, .) %>%
    mutate(sample_id = paste(subject_id, region, sep = "_")) %>%
    column_to_rownames("sample_id")

# concentration data
sheets = c(
    "LcBC" = "LcBC_AbsAbund_raw",
    "MTC" = "MTC_AbsAbund_raw",
    "LPFC" = "LPFC_AbsAbund_raw"
)

dat_list = lapply(names(sheets), function(region){
    sheet = sheets[region]
    read_excel(file, sheet = sheet)
})
efdata = NULL
for(dat in dat_list){
    if(is.null(efdata)){
        efdata = dat
    } else{
        efdata = merge(efdata, dat, all = TRUE)
    }
}
rownames(efdata) = efdata$`Glycan-Type`

edata = efdata[,grep("^[0-9]", colnames(efdata))]
fdata = efdata[,!colnames(efdata) %in% colnames(edata)]
colnames(edata) = sub("_\\d", "_", toupper(colnames(edata)))

edata = edata[, rownames(pdata)]
edata = as.matrix(edata)
edata[is.na(edata)] = 0

abs_abund_raw <- HTSet(edata = edata, fdata = fdata, pdata = pdata)

sheets_mgs <- c(
    "LcBC_mg" = "LcBC_AbsAbund_Norm-mgtissue",
    "MTC_mg" = "MTC_AbsAbund_Norm-mgtissue",
    "LPFC_mg" = "LPFC_AbsAbund_Norm-mgtissue"
)

dat_list2 = lapply(names(sheets_mgs), function(region){
    sheet_mgs = sheets_mgs[region]
    read_excel(file, sheet = sheet_mgs)
})
efdata2 = NULL
for(dat in dat_list2){
    if(is.null(efdata2)){
        efdata2 = dat
    } else{
        efdata2 = merge(efdata2, dat, all = TRUE)
    }
}
rownames(efdata2) = efdata2$`Glycan-Type`

edata_mgs = efdata2[,grep("^[0-9]", colnames(efdata2))]
fdata_mgs = efdata2[,!colnames(efdata2) %in% colnames(edata_mgs)]
colnames(edata_mgs) = sub("_\\d", "_", toupper(colnames(edata_mgs)))

edata_mgs = edata_mgs[, rownames(pdata)]
edata_mgs = as.matrix(edata_mgs)
edata_mgs[is.na(edata_mgs)] = 0
abs_abund_mgs <- HTSet(edata = edata_mgs, fdata = fdata_mgs, pdata = pdata)

data <- list(abs_raw = abs_abund_raw, abs_mgs = abs_abund_mgs)
saveRDS(data, file = "case_study2.rds")
