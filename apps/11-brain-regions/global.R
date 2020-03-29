pkgs=c("dplyr", "reshape2", "HTSet", "ggplot2", "plotly", "DT", "shiny", 
       "shinydashboard", "glue", "R6", "tibble", "ggsci", "shinyjqui", "limma",
       "edgeR")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

# shiny module skeleton
import::here(ShinyModule, .from="ShinyModule.R")

# set default ggplot theme
theme_set(theme_bw())
.DATA_RDS_PATH = "data/data.rds"

# initialize the DataModel
import::here(DataModel, .from="models/DataModel.R")
.DATA = DataModel$new()

`%+%` = function(e1, e2){
    if(is.numeric(e1) & is.numeric(e2)) return(e1 + e2)
    if(is.character(e1) & is.character(e2)) return(paste0(e1, e2))
    stop("unsupported data type")
}