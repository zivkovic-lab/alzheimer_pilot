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
                    menuItem("Differential Expression", tabName = "de"),
                    menuItem("Enrichment FET", tabName = "fet"),
                    menuItem("Enrichment GSEA", tabName = "gsea")
                )
            )
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)