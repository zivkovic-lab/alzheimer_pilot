# set the file location as the working directory
setwd(dirname(parent.frame(2)$ofile))

# load required packages
pkgs = c("tidyverse", "reshape2", "HTSet", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# data path
file = "../raw-data/20201111_VennAIPBNet.peak-areas-only.abundance-all batches_F2.xlsx"


# -------------------------------------------------------------------------
# pdata
# -------------------------------------------------------------------------
covariates <- read_csv("../raw-data/Biosample_List_2019018_FinalExt_djh.csv") %>%
    mutate(Blood_Draw_Dt = as.POSIXct(Blood_Draw_Dt, format="%m/%d/%Y", tz = 'UTC'))
pdata <- read_excel(file, sheet = 3) %>%
    as.data.frame %>%
    dplyr::rename_with(~ gsub(" ", "_", .)) %>%
    full_join(covariates, by = c("Specimen_Type", "Blood_Draw_Dt", "Specimen_Label", "Dx_Group")) %>%
    mutate(SuperDx = ifelse(grepl("^AD", Dx_Group), "AD", "Normal")) %>%
    mutate(
        Dx_Group = factor(Dx_Group, 
                          levels = c("Normal Clinical Dx Only", "AD Clinical Dx Only", "AD Autopsy Confirmed")),
        SuperDx = factor(SuperDx, levels = c("Normal", "AD")),
        adc_gender = factor(adc_gender, levels = c("Male", "Female")),
        adc_ethnicity = factor(adc_ethnicity),
        # apoe: present/absent of E4
        apoe4 = factor(ifelse(apoe %in% c(1,3,6), "absent", "present")),
        adc_ethnicity_umb = factor(ifelse(adc_ethnicity == "White", "Caucasian", "Non-caucasian")),
        diabetes = factor(ifelse(is.na(diabetes), "unknown", diabetes))
    ) %>%
    column_to_rownames("Sample_Code")


# -------------------------------------------------------------------------
# edata
# -------------------------------------------------------------------------
edata <- read_excel(file, sheet = 1) %>%
    as.data.frame %>% 
    tibble::column_to_rownames("Sample code") %>%
    t()
identical(rownames(pdata), colnames(edata)) #Does sample code match?
edata <- edata[, rownames(pdata)]

## Peptides (start with "pep-")
edata_pep <- edata[grepl("^pep-", rownames(edata)),]
# rownames(edata_pep) <- gsub("^pep-", "", rownames(edata_pep))
fdata_pep <- str_split_fixed(rownames(edata_pep), "_", n = 2) %>%
    as.data.frame %>% 
    data.table::setnames(c("protein", "sequence"))
rownames(fdata_pep) <- rownames(edata_pep)

peptide <- HTSet(edata = edata_pep, fdata = fdata_pep, pdata = pdata)

## Glycopeptides
edata_glycan <- edata[!grepl("^pep-", rownames(edata)),]
fdata_glycan <- str_split_fixed(rownames(edata_glycan), "_", n = 3) %>%
    as.data.frame %>% 
    data.table::setnames(c("protein", "position", "composition"))
rownames(fdata_glycan) = rownames(edata_glycan)
# Structure
group <- read_excel(file, sheet = 5) %>%
    filter(`Glycan Subtype` != "NG")
rowsinfdata <- match(group$`Sample code`, rownames(fdata_glycan))
fdata_glycan$subtype <- "NG"
fdata_glycan$subtype[rowsinfdata] <- group$`Glycan Subtype`
glycopeptide <- HTSet(edata = edata_glycan, fdata = fdata_glycan, pdata = pdata)

## Peptides (intensity)
edata_pep_intense <- read_excel(file, sheet = 2) %>%
    select(-1) %>%
    as.data.frame %>% 
    tibble::column_to_rownames("Sample code") %>%
    t()
identical(rownames(pdata), colnames(edata_pep_intense)) #Does sample code match?
edata_pep_intense <- edata_pep_intense[, rownames(pdata)]
### fdata_pep_intense
fdata_pep_intense <- str_split_fixed(rownames(edata_pep_intense), "_", n = 2) %>%
    as.data.frame %>% 
    data.table::setnames(c("protein", "sequence"))
rownames(fdata_pep_intense) <- rownames(edata_pep_intense)
peptide_intense <- HTSet(edata_pep_intense, fdata_pep_intense, pdata)

## save
serum <- list(
    peptide = peptide,
    glycopeptide = glycopeptide,
    peptide_intensity = peptide_intense
)
saveRDS(serum, "serum.rds")
saveRDS(serum, "../R/explor/data/serum.rds")
