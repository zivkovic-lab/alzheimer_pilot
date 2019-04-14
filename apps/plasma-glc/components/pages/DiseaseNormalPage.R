#import::here(LinearModelPanel, "../modules/LinearModelPanle.R")
import::here(FormulaPanel, .from = "../modules/FormulaPanel.R")
#import::here(StatsTable, .from = "../modules/StatsTable.R")

DiseaseNormalPage = R6Class(
    "DiseaseNormalPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "da-normal",
        formulaPanel = NULL,
        statsTable = NULL,
        
        # initializer
        initialize = function(){
            self$formulaPanel = FormulaPanel$new("formula-panel", "da-normal")
            #self$statsTable = StatsTable$new()
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            
            tagList(
                column(
                    width = 6,
                    box(
                        width = NULL,
                        self$formulaPanel$ui()
                    )
                    #self$statsTable$ui()
                )
            )
        },
        
        # server
        server = function(input, output, session){
            
            
        },
        
        fit = function(formula){
            mset = subset_samples(glc, !is.na(glc$sample_table$group))
            design = model.matrix(
                data = as(mset$sample_table, "data.frame"),
                formula
            )
        }
    )
)