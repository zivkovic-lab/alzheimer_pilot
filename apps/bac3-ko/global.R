pkgs=c("dplyr", "reshape2", "stringr", "HTSet", "clusterProfiler", "ggsci",
       "shiny", "shinydashboard", "ggplot2", "plotly", "DT", "R6",
       "shiny.router", "shinyauthr")
for(pkg in pkgs){
    suppressPackageStartupMessages(library(pkg, character.only=TRUE))
}

dashboardHeader = eval(parse(text = deparse(shinydashboard::dashboardHeader)[-5]))

source("ShinyModule.R")

import::here(DataModel, .from = "models/DataModel.R")
.DATA = DataModel$new("data/data.rds")

import::here(AuthModel, .from = "models/AuthModel.R")
.AUTH = AuthModel$new()

mito_ptws = c(
    "mmu04137", "mmu04723", "mmu04931", "mmu04022", "mmu04927", "mmu04714",
    "mmu04922", "mmu00062", "mmu00190", "mmu00230", "mmu01212", "mmu04621",
    "mmu04727", "mmu00020", "mmu00061", "mmu04142", "mmu04152", "mmu04211",
    "mmu04218", "mmu04371", "mmu04622", "mmu04623", "mmu04724", "mmu04725"
)

apop_ptws = c(
    "mmu04215", "mmu04210", "mmu05222", "mmu04668", "mmu03050", "mmu05146",
    "mmu04068", "mmu04115", "mmu04350", "mmu01521", "mmu04151", "mmu04217",
    "mmu04392", "mmu04540", "mmu04933", "mmu04390", "mmu04512", "mmu04621"
)
