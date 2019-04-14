pkgs = c("dplyr", "reshape2", "stringr","ggplot2", "Metabase", "ggmetaplots",
         "shiny", "shinydashboard", "R6", "DT", "plotly", "shinyjs")

for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = T))
}

load("data/data.rda")

import::here(ShinyModule, .from="components/modules/ShinyModule.R")