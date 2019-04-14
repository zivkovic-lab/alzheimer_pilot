Header = R6Class(
    "Header",
    inherit = ShinyModule,
    public = list(
        # UI
        ui = function(){
            dashboardHeader(
                title = "Plasma Glycopeptides"
            )
        }
    )
)