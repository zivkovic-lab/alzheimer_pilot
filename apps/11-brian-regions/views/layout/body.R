import::here(PagePCA, .from="../pages/PCA.R")
import::here(PageBarplot, .from="../pages/barplot.R")
import::here(PageEA, .from="../pages/EA.R")
import::here(PageLM, .from="../pages/lm.R")

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        pagePCA = NULL,
        pageBarplot = NULL,
        pageLM = NULL,
        pageEA = NULL,
        
        # initializer
        initialize = function(){
            self$pagePCA = PagePCA$new()
            self$pageBarplot = PageBarplot$new()
            self$pageLM = PageLM$new()
            self$pageEA = PageEA$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel="stylesheet"),
                tabItems(
                    tabItem("pca", self$pagePCA$ui()),
                    tabItem("bar", self$pageBarplot$ui()),
                    tabItem("lm", self$pageLM$ui()),
                    tabItem("ea", self$pageEA$ui())
                )
            )
        },
        
        # server
        server = function(input, output, session){
            self$pagePCA$call()
            self$pageBarplot$call()
            self$pageLM$call()
            self$pageEA$call()
        }
    )
)