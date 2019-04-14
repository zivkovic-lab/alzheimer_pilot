import::here(DiseaseNormalPage, .from="../pages/DiseaseNormalPage.R")

Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        diseaseNormalPage = NULL,
        
        # initializer
        initialize = function(){
            self$diseaseNormalPage = DiseaseNormalPage$new()  
        },
        
        # UI
        ui = function(){
            dashboardBody(
                shinyjs::useShinyjs(),
                fluidPage(
                    tabItems(
                        tabItem("ad-normal", self$diseaseNormalPage$ui())
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            
            
        }
    )
)