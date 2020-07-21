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
                    menuItem("Overview", tabName = "overview"),
                    menuItem("PCA Plot", tabName = "pca"),
                    menuItem("Clustering", tabName = "cluster"),
                    menuItem("Glycan Barplot", tabName = "bar"),
                    menuItem("Linear Model", tabName = "lm"),
                    menuItem("Enrichment Analysis", tabName="ea"),
                    menuItem("Enrichment Analysis 2", tabName="ea2"),
                    menuItem("Enrichment Analysis 3", tabName="ea3")
                )
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)