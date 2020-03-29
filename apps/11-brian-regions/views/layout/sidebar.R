Sidebar = R6Class(
    "Sidebar",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    menuItem("PCA Plot", tabName = "pca"),
                    menuItem("Glycan Barplot", tabName = "bar"),
                    menuItem("Linear Model", tabName = "lm"),
                    menuItem("Enrichment Analysis", tabName="ea")
                )
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)