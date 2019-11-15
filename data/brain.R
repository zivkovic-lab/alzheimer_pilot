#' This is Jennifer's study, glycomix in two AD and two control patients, 11 
#' brain ragions
setwd(dirname(parent.frame(2)$ofile))
pkgs=c("dplyr", "reshape2", "stringr", "tibble", "tidyr", "Metabase", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

file = "../data-raw/All E1_20191109_NGlycomics.xlsx"

# subject codes
pdata = read_excel(file, sheet = 3, na = 'n/a') %>%
    as.data.frame %>% 
    select(-`...2`) %>%
    data.table::setnames(old = "...1", "region") %>%
    melt(id.vars = "region") %>%
    separate(variable, into = c("group", "age"), sep = " ") %>%
    mutate(age = as.integer(gsub('\\([0-9]+\\)', "\\1", age))) %>%
    filter(!is.na(value)) %>%
    column_to_rownames("value")
 
# sheet 1
abs_abund = read_excel(file, sheet = 1)
fdata = abs_abund[,1:7] %>%
    as.data.frame %>%
    mutate(name = paste(Class, Glycan, sep="_")) %>%
    column_to_rownames("name")
edata = abs_abund[,8:45] %>%
    as.matrix %>%
    `rownames<-`(rownames(fdata)) %>%
    `[`(,rownames(pdata))

abs_abund = GlycomicsSet(
    conc_table = conc_table(edata),
    feature_data = feature_data(fdata),
    sample_table =sample_table(pdata)
)

# sheet 2
rel_abund = read_excel(file, sheet = 2)
fdata = rel_abund[,1:7] %>%
    as.data.frame %>%
    mutate(name = paste(Class, Glycan, sep="_")) %>%
    column_to_rownames("name")
edata = rel_abund[,8:45] %>%
    as.matrix %>%
    `rownames<-`(rownames(fdata)) %>%
    `[`(,rownames(pdata))

rel_abund = GlycomicsSet(
    conc_table = conc_table(edata),
    feature_data = feature_data(fdata),
    sample_table =sample_table(pdata)
)

data = list(
    abs_abund = abs_abund,
    rel_abund = rel_abund
)
save(data, file = "brain.rda")