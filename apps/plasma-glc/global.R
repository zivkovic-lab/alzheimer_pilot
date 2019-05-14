pkgs = c("dplyr", "reshape2", "stringr","ggplot2", "Metabase", "ggmetaplots",
         "shiny", "shinydashboard", "R6", "DT", "plotly", "shinyjs", "glue",
         "tibble")

for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only = T))
}

load("data/data.rda")

import::here(ShinyModule, .from="components/modules/ShinyModule.R")

theme_set(theme_bw())

#glc = transform_by_feature(glc, log2)

transform_choices = c('none', 'log', 'log(x+1)', 'square', 'cubic', 'square root')