setwd(dirname(parent.frame(2)$ofile))

pkgs=c("dplyr", "reshape2", "tibble", "stringr", "Metabase", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

file = "../data-raw/CaseStudy_AD_20191116_R&A.xlsx"

# subject code
ranges = c(
    "Medial Temporal Cortex" = "A2:B16",
    "Lateral Pre-Frontal Cortex" = "D2:E20",
    "Lateral Cerebellar Cortex" = "G2:H18"
)
pdata = lapply(names(ranges), function(region){
    range = ranges[region]
    df = read_excel(file, sheet = "Sample Info", range = range,
                    col_names = c("sample_id", "group"))
    df$sample_id = toupper(df$sample_id)
    df$subject_id = str_split_fixed(df$sample_id, "_", n = 2)[,1]
    df$region = str_split_fixed(df$sample_id, "_", n = 2)[,2]
    df$region_fullname = region
    return(df)
}) %>%
    do.call(rbind, .) %>%
    column_to_rownames("sample_id")

# concentration data
sheets = c(
    "MTC" = "MTC_AbsAbund",
    "LPFC" = "LPFc_AbsAbund",
    "LPBC" = "LPbC_AbsAbund"
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
rownames(efdata) = efdata$Glycan

edata = efdata[,grep("^[0-9]", colnames(efdata))]
fdata = efdata[,!colnames(efdata) %in% colnames(edata)]
colnames(edata) = toupper(colnames(edata))

edata = edata[, rownames(pdata)]
edata = as.matrix(edata)
edata[is.na(edata)] = 0

data = GlycomicsSet(
    conc_table = conc_table(edata),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata)
)

saveRDS(data, file = "case_study.rds")