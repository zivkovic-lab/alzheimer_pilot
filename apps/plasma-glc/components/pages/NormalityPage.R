NormalityPage = R6Class(
    "NormalityPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "normality",
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            
            tagList(
                
            )    
        },
        
        # server
        server = function(input, output, session){
        
        }
    )
)