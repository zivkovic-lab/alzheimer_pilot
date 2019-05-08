# set the file location as the working directory
setwd(dirname(parent.frame(2)$ofile))

# load required packages
pkgs = c("dplyr", "stringr", "reshape2", "Metabase", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# data path
file = "../../../data-raw/AD plasma - normalized data.xlsx"

# -------- sample table --------------------------------------------------------
# read the pdata
pdata = read_excel(file, sheet = 3) %>% as.data.frame %>%
    tibble::column_to_rownames("SAMPLE CODE")
pdata$`Clinical Group` = ifelse(
    is.na(pdata$PATHDX1), ifelse(
        grepl("^AD", pdata$GROUP), "AD", "normal"
    ), ifelse(
        grepl("^01", pdata$PATHDX1), "normal", "AD"
    )
)
# data cleaning
pdata$`Clinical Group` = factor(pdata$`Clinical Group`, levels = c("normal", "AD"))
pdata$`ApoE (1)` = ifelse(
    is.na(pdata$`ApoE (1)`), 
    as.integer(gsub(".* E(\\d)/.*", "\\1", pdata$GROUP)),
    pdata$`ApoE (1)`
)
pdata$`ApoE (1)` = factor(pdata$`ApoE (1)`)
pdata$`ApoE (2)` = ifelse(
    is.na(pdata$`ApoE (2)`),
    as.integer(gsub(".*(\\d)$", "\\1", pdata$GROUP)),
    pdata$`ApoE (2)`
)
pdata$`ApoE (2)` = factor(pdata$`ApoE (2)`)
pdata$apoE = paste(pdata$`ApoE (1)`, pdata$`ApoE (2)`, sep = "-")


# -------- istd corrected peptides ---------------------------------------------
edata = read_excel(file, sheet = 1, na = "ND") %>%
    as.data.frame %>% tibble::column_to_rownames("...1")
edata = edata[,rownames(pdata)]
pdata$ISTD = as.numeric(edata[1,])
edata = edata[-1,]

# peptides
edata1 = edata[grepl("^QuantPep-", rownames(edata)),]
rownames(edata1) = gsub("^QuantPep-", "", rownames(edata1))

fdata1 = str_split_fixed(rownames(edata1), "_", n = 2) %>%
    as.data.frame %>% data.table::setnames(c("protein", "sequence"))
rownames(fdata1) = rownames(edata1)

ppt = ProteomicsSet(
    conc_table = conc_table(as.matrix(edata1)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata1)
)

# glycpeptides
edata2 = edata[!grepl("^QuantPep-", rownames(edata)),]

fdata2 = str_split_fixed(rownames(edata2), "_", n = 3) %>%
    as.data.frame %>% data.table::setnames(c("protein", "position", "composition"))
rownames(fdata2) = rownames(edata2)

glc1 = ProteomicsSet(
    conc_table = conc_table(as.matrix(edata2)),
    sample_table = sample_table(pdata),
    feature_data = feature_data(fdata2)
)

# -------- normalized glycopeptides --------------------------------------------
# read the edata
edata = read_excel(file, sheet = 2, na = "ND") %>%
    as.data.frame %>%
    tibble::column_to_rownames("...1")

# rearrange the edata
edata = edata[,rownames(pdata)] %>% as.matrix

# create a fdata
fdata = str_split_fixed(rownames(edata), "\\_", n=3) %>%
    as.data.frame %>%
    data.table::setnames(c("protein", "position", "composition"))
rownames(fdata) = rownames(edata)

# construct the GlycomicsSet object
glc2 = GlycomicsSet(
    conc_table(edata),
    sample_table(pdata),
    feature_data(fdata)
)

# save it
data = list(
    peptides = ppt,
    glycopeptides_adjusted = glc1,
    glycopeptides_normalized = glc2
)
save(data, file = "data.rda")