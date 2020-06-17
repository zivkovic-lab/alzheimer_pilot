import::here(OverviewPage, .from = "../tabs/overview.R")
import::here(DEPage, .from = "../tabs/de.R")
import::here(FETPage, .from = "../tabs/fet.R")
import::here(GSEAPage, .from = "../tabs/gsea.R")

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        overviewPage = NULL,
        dePage = NULL,
        fetPage = NULL,
        gseaPage = NULL,
        
        # initializer
        initialize = function(){
            self$overviewPage = OverviewPage$new()
            self$dePage = DEPage$new()
            self$fetPage = FETPage$new()
            self$gseaPage = GSEAPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel="stylesheet"),
                shinyjs::useShinyjs(),
                tabItems(
                    tabItem("overview", self$overviewPage$ui()),
                    tabItem("de", self$dePage$ui()),
                    tabItem("fet", self$fetPage$ui()),
                    tabItem("gsea", self$gseaPage$ui())
                )
            )
        },
        
        # server
        server = function(input, output, session){
            self$overviewPage$call()
            self$dePage$call()
            self$fetPage$call()
            self$gseaPage$call()
        }
    )
)