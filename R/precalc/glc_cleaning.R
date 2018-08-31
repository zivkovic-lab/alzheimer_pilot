## --------------------- load packages -----------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', "readxl","data.table")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
## -------------------- edata and fdata ----------------------
rm(list=ls())
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Alzheimer_Disease_Study")

file='data/FFS AD data normalized.xlsx'

fdata = read_excel(path = file, sheet = "SNR 10", range = "A1:G104") %>%
    as.data.frame %>%
    setnames(old=c("X__1","N/O glycn"),new=c("rownames", "N/O glycan")) %>%
    mutate(
        Protein = gsub("H2HSG","A2HSG", Protein),
        rownames = str_c(Protein, Site, 
                         str_replace_na(`N/O glycan`, ""), 
                         str_replace_na(`Composition (Hex HexNAc Fuc Sia)`,""),
                         sep="_"),
        rownames = ifelse(
            Protein == "ApoE",
            rownames,
            str_c(rownames, 
                  str_replace_na(Comments,""), sep = "_")
        ),
        
        rownames = gsub("\\_{1,3}$","",rownames, perl = T),
        rownames = gsub("Miscleavage","Misc",rownames),
        
        # rownames = gsub("\\ Results","", rownames),
        # rownames = gsub("LGPLVEQGR", "ApoE", rownames),
        # rownames = gsub("Apo_","ApoC3_", rownames),
        
        Normalization = gsub("Apo ","ApoC3 ", Normalization),
        Normalization = gsub("LGPLVEQGR", "ApoE", Normalization)
    ) %>%
    column_to_rownames(var = "rownames")

# Read in the edata
edata = read_excel(path = file, sheet = "SNR 10", 
                   range = "AW1:BK104", na = "ND") %>%
    as.data.frame %>%
    mutate(rownames = rownames(fdata)) %>%
    column_to_rownames(var = "rownames")

## ------------------------ pdata ---------------------------
pdata = read_excel(
    path = "data/BiospecimenData_BioReq2018003.xlsx",
    sheet = 1, range = "A2:Q16", col_names = FALSE
) 
colnames(pdata) = c(
    read_excel(
        path = "data/BiospecimenData_BioReq2018003.xlsx",
        sheet = 1, range = "A1:I1", col_names = FALSE
    ) %>% as.character,
    "NP SCORE A", "NP SCORE B", "NP SCORE C",
    read_excel(
        path = "data/BiospecimenData_BioReq2018003.xlsx",
        sheet = 1, range = "M1:Q1", col_names = FALSE
    ) %>% as.character
)
pdata = as.data.frame(pdata) %>% 
    remove_rownames() %>%
    column_to_rownames(var="SAMPLE ID")

pdata_part2 = read_excel(
    path = "data/Alzheimer sample IDs_HDL total prot.xlsx",
    sheet = 1, range = "A1:D16"
) %>% as.data.frame %>%
    remove_rownames() %>%
    column_to_rownames(var = "Subject IDs")

pdata = merge(pdata, pdata_part2, by=0, all=TRUE) %>%
    column_to_rownames(var = "Row.names")

pdata$GROUP = factor(pdata$GROUP)
pdata$GROUP = relevel(pdata$GROUP, "CTRL E3/E3")
pdata$SEX = factor(pdata$SEX)

pdata = pdata[order(pdata$GROUP),]

edata = edata[,rownames(pdata)]

## ----------------- peptide and glycan ----------------------
glycopeptide = list(
    peptides = list(
        fdata = fdata[is.na(fdata$`N/O glycan`),1:2],
        edata = edata[is.na(fdata$`N/O glycan`),],
        pdata = pdata
    ),
    glycans = list(
        fdata = fdata[!is.na(fdata$`N/O glycan`),],
        edata = edata[!is.na(fdata$`N/O glycan`),],
        pdata = pdata
    )
)

## ------------------- missing values ------------------------

## Not run:
# ht = apply(edata,1, function(xx) sum(is.na(xx))) %>% table
# bp = barplot(ht, col = RSkittleBrewer::RSkittleBrewer("tropical")[2], ylim=c(0,48))
# text(bp, ht + 2, ht)

# num_na = apply(edata, 1, function(row){
#     sum(is.na(row))
# })
# edata = edata[num_na <= 5,]
# fdata = fdata[rownames(edata),]
# 
# fdata = fdata[rownames(fdata) != "GLC095",]
# edata = edata[rownames(fdata),]

## ---------------------- save out ---------------------------
save(glycopeptide, file="R/Rdata/glc.Rdata")


