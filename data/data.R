## --------------------- load packages -----------------------
pkgs = c('dplyr','stringr','reshape2','tibble', "readxl","data.table", "Metabase")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
setwd(dirname(parent.frame(2)$ofile))
################################################################################
##########                     G L Y C O M I C S                      ##########
################################################################################
## -------------------- edata and fdata ----------------------

file='../data-raw/FFS AD data normalized.xlsx'

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
    path = "../data-raw/BiospecimenData_BioReq2018003.xlsx",
    sheet = 1, range = "A2:Q16", col_names = FALSE
) 
colnames(pdata) = c(
    read_excel(
        path = "../data-raw/BiospecimenData_BioReq2018003.xlsx",
        sheet = 1, range = "A1:I1", col_names = FALSE
    ) %>% as.character,
    "NP SCORE A", "NP SCORE B", "NP SCORE C",
    read_excel(
        path = "../data-raw/BiospecimenData_BioReq2018003.xlsx",
        sheet = 1, range = "M1:Q1", col_names = FALSE
    ) %>% as.character
)
pdata = as.data.frame(pdata) %>% 
    remove_rownames() %>%
    column_to_rownames(var="SAMPLE ID")

pdata_part2 = read_excel(
    path = "../data-raw/Alzheimer sample IDs_HDL total prot.xlsx",
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

pdata = pdata %>%
    rownames_to_column("sample_id") %>%
    mutate(
        Diagnosis = ifelse(grepl("^CTRL", GROUP), "Control", "AD"),
        `ApoE Genotype` = gsub(".+(E[34]/E[34])", "\\1", GROUP)
    ) %>% 
    setnames(old = c("GROUP", "AGE", "SEX"), c("Group", "Age", "Gender")) %>%
    column_to_rownames("sample_id")

## ----------------- peptide and glycan ----------------------

glc = list(
    peptide = MultxSet(
        conc_table = edata[is.na(fdata$`N/O glycan`),] %>% as.matrix %>% conc_table,
        sample_table = sample_table(pdata),
        feature_data = fdata[is.na(fdata$`N/O glycan`),1:2] %>% feature_data,
        experiment_data = MultiExperimentData(experiment_type = "Unglycosylated Peptides")
    ),
    glycans = GlycomicsSet(
        conc_table = edata[!is.na(fdata$`N/O glycan`),] %>% as.matrix %>% conc_table,
        sample_table = sample_table(pdata),
        feature_data = fdata[!is.na(fdata$`N/O glycan`),1:2] %>% feature_data
    )
)
glc$glycans = subset_features(
    glc$glycans, 
    apply(glc$glycans$conc_table, 1, function(x) sum(is.na(x)) <= 5)
)
glc$glycans = transform_by_feature(glc$glycans, function(x) 
    ifelse(is.na(x), min(x, na.rm = TRUE)/2, x))

################################################################################
##########                  H D L   F U N C T I O N                   ##########
################################################################################
file = "../data-raw/AD Patients HDL Glycans (with 18 subtracted from all ApoE sites), microglia % efflux 3-22-18.xlsx"

fct = read_excel(path = file, sheet = "AD HDL Glycopeptides (2)",
                 range = "A1:I16") %>%
    as.data.frame %>%
    column_to_rownames("Subject ID")
fct = fct[sampleNames(glc$peptide),]

fct = MultxSet(
    conc_table = fct[,6:8] %>% t %>% conc_table,
    sample_table = fct[,1:5] %>% sample_table,
    experiment_data = MultiExperimentData(experiment_type = "HDL Function")
)
fct$sample_table$Group = factor(
    fct$sample_table$Group, 
    levels = c("CTRL E3/E3", "AD   E3/E3", "AD   E3/E4")
)

## ---------------------- save out ---------------------------
save(glc, fct, file="hdl.rda")


