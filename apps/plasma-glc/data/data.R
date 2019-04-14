# set the file location as the working directory
setwd(dirname(parent.frame(2)$ofile))

# load required packages
pkgs = c("dplyr", "stringr", "reshape2", "Metabase", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# data path
file = "../../../data-raw/AD plasma - normalized data.xlsx"

# read the edata
edata = read_excel(file, sheet = 2, na = "ND") %>%
    as.data.frame %>%
    tibble::column_to_rownames("...1")

# read the pdata
pdata = read_excel(file, sheet = 3) %>% as.data.frame %>%
    tibble::column_to_rownames("SAMPLE CODE")
pdata$group = ifelse(
        is.na(pdata$PATHDX1), NA, ifelse(
            grepl("^01", pdata$PATHDX1), "normal", "AD"
        )
    )

# rearrange the edata
edata = edata[,rownames(pdata)] %>% as.matrix

# create a fdata
fdata = str_split_fixed(rownames(edata), "\\_", n=3) %>%
    as.data.frame %>%
    data.table::setnames(c("protein", "position", "composition"))
rownames(fdata) = rownames(edata)

# construct the GlycomicsSet object
glc = GlycomicsSet(
    conc_table(edata),
    sample_table(pdata),
    feature_data(fdata)
)

# save it
save(glc, file = "data.rda")