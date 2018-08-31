pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase',
         'ggsci', "shiny", "shinydashboard", "ggmetaplots", "heatmaply",
         "RColorBrewer")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load("../../R/precalc/data.rda")
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
    header = dashboardHeader(title = "Alzheimer Pilot Study"),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("ui/inputs.R", local = TRUE)
    
    source("server/glc/boxplot.R", local = TRUE)
    source("server/glc/hist.R", local = TRUE)
    source("server/glc/corr_fct.R", local = TRUE)
    source("server/fct/boxplot.R", local = TRUE)
}

shinyApp(ui = ui, server = server)