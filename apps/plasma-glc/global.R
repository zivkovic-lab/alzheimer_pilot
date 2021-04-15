# pkgs = c("dplyr", "reshape2", "stringr","ggplot2", "Metabase", "ggmetaplots",
#          "shiny", "shinydashboard", "R6", "DT", "plotly", "shinyjs", "glue",
#          "tibble")
# 
# for(pkg in pkgs){
#     suppressPackageStartupMessages(library(pkg, character.only = T))
# }

library(tidyverse)
library(reshape2)
library(Metabase)
library(ggmetaplots)
library(plotly)
library(DT)
library(limma)
library(edgeR)
library(shiny)
library(shinydashboard)
library(R6)
library(shinyjs)
library(glue)

load("data/data.rda")

import::here(ShinyModule, .from="components/modules/ShinyModule.R")

theme_set(theme_bw())

#glc = transform_by_feature(glc, log2)

transform_choices = c('none', 'log', 'log(x+1)', 'square', 'cubic', 'square root')