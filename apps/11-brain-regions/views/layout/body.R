import::here(PageOverview, .from="../pages/overview.R")
import::here(PagePCA, .from="../pages/PCA.R")
import::here(PageBarplot, .from="../pages/barplot.R")
import::here(PageEA, .from="../pages/EA.R")
import::here(PageLM, .from="../pages/lm.R")
import::here(PageEA2, .from="../pages/EA2.R")

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        pageOverview = NULL,
        pagePCA = NULL,
        pageBarplot = NULL,
        pageLM = NULL,
        pageEA = NULL,
        pageEA2 = NULL,
        
        # initializer
        initialize = function(){
            self$pageOverview = PageOverview$new()
            self$pagePCA = PagePCA$new()
            self$pageBarplot = PageBarplot$new()
            self$pageLM = PageLM$new()
            self$pageEA = PageEA$new()
            self$pageEA2 = PageEA2$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel="stylesheet"),
                tabItems(
                    tabItem("overview", self$pageOverview$ui()),
                    tabItem("pca", self$pagePCA$ui()),
                    tabItem("bar", self$pageBarplot$ui()),
                    tabItem("lm", self$pageLM$ui()),
                    tabItem("ea", self$pageEA$ui()),
                    tabItem("ea2", self$pageEA2$ui())
                )
            )
        },
        
        # server
        server = function(input, output, session){
            self$pageOverview$call()
            self$pagePCA$call()
            self$pageBarplot$call()
            self$pageLM$call()
            self$pageEA$call()
            self$pageEA2$call()
        }
    )
)