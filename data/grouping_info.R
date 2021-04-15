setwd(dirname(parent.frame(2)$ofile))
pkgs=c("tidyverse", "HTSet", "readxl")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

subtype <- read_excel(
    "../raw-data/Data grouping_AD Case Study_20210111.xlsx", 
    range = "A3:B29", 
    col_names = c('Class', 'Subtypes')
)
grouping <- list(subtype = subtype)
saveRDS(grouping, file = "grouping.rds")
