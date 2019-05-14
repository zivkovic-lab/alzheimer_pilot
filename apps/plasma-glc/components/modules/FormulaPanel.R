import::here(FormulaVarSelector, .from="FormulaVarSelector.R")

FormulaPanel = R6Class(
    "FormulaPanel",
    inherit = ShinyModule,
    public = list(
        # attributes
        parent_id = NULL,
        
        # initializer
        initialize = function(id, parent_id){
            self$id = id
            self$parent_id = parent_id
        },
        
        # UI
        ui = function(){
            ns = NS(NS(self$parent_id)(self$id))
            tagList(
                fluidRow(
                    # Button to add a variable
                    column(
                        width = 12,
                        actionButton(
                            ns("add-var"),
                            "Add Variable",
                            class = "btn-primary"
                        )
                    ),
                    # Variable selector
                    column(
                        width = 12,
                        uiOutput(ns("var-inputs"))   
                    ),
                    column(
                        width = 12,
                        actionButton(ns("vars-confirm"), "Confirm", class="btn-primary"),
                        tags$br(),
                        tags$hr()
                    ),
                    column(
                        width = 12,
                        uiOutput(ns("formula-ui")),
                        actionButton(ns("formula-submit"), "Submit", class="btn-primary"),
                        tags$br(),
                        tags$hr()
                    ),
                    column(
                        width = 6,
                        uiOutput(ns("coef-ui")),
                        actionButton(ns("coef-submit"), "Submit", class="btn-primary")
                    ),
                    column(
                        width = 6,
                        selectInput(
                            ns('transform'), 'Transform method',
                            choices = transform_choices,
                            selected = transform_choices[2]
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session, props){
            # state variables inside of this module
            states = reactiveValues(
                varNum = 0,
                varNames = list(),
                varTypes = list(),
                varSelect = list(),
                formula = NULL,
                submitted = NULL
            )
            # variables to emit to the parent module
            emit = reactiveValues(
                design = NULL,
                coef = NULL
            )
            
            # event listener to add a variable
            observeEvent(input$`add-var`, {
                states$varNum = states$varNum + 1
                id = glue("var{states$varNum}")
                states$varSelect[[id]] = FormulaVarSelector$new(
                    id = id,
                    parent_id = NS(self$parent_id)(self$id),
                    varChoices = colnames(data$peptides$sample_table),
                    varName = states$VarNames[[id]],
                    varType = states$varTypes[[id]]
                )
            })
            
            # var-inputs
            output$`var-inputs` = renderUI({
                tags$table(
                    class = "table",
                    tags$tr(
                        tags$th(scope = "col", "Variable"),
                        tags$th(scope = "col", "data type"),
                        tags$th(scope = "col")
                    ),
                    tags$tbody(
                        lapply(seq_len(states$varNum), function(i){
                            states$varSelect[[glue("var{i}")]]$ui()
                        })
                    )
                )
            })
            
            observeEvent(input$`vars-confirm`, {
                for(i in seq_len(states$varNum)){
                    id = glue("var{i}")
                    emitData = states$varSelect[[id]]$call()
                    logjs(emitData$varName)
                    states$varNames[[id]] = emitData$varName
                    states$varTypes[[id]] = emitData$varType
                    states$varSelect[[id]]$varName = emitData$varName
                    states$varSelect[[id]]$varType = emitData$varType
                }
                showNotification("Variables confirmed!", type = "message")
            })
            
            # formula input UI
            observe({
                
                output$`formula-ui` = renderUI({
                    formula = glue("~ `{paste(states$varNames, collapse = '` + `')}`")   
                    textInput(
                        session$ns("formula"), 
                        tags$p(
                            "Input formula",
                            tags$div(
                                class = "help-tip",
                                tags$p("The formula must be in the form of \"~ factor1 + factor2\". Each variable must present as a column name of the sample table. If any special character present in the variable name such as space or dash, use the `` symble. ")
                            )
                        ),
                        value = formula, width = "100%"
                    )
                })
            })
            
            # event listener fro formula submit
            observeEvent(input$`formula-submit`, {
                if(!self$formulaValidator(input$formula)){
                    addClass("formula", class = "invalid")
                } else {
                    states$formula = as.formula(input$formula)
                    invalidVar = self$variableValidator(states$formula, states$varNames)
                    if(length(invalidVar) != 0){
                        showNotification(
                            glue("Variable not confirmed: {paste(invalidVar, collapse = ', ')}"),
                            type = "error"
                        )
                    } else {
                        emit$design = self$getDesignMatrix(
                            data[[props$dataset]], states$varNames,
                            states$varTypes, states$formula
                        )
                    }
                }
            })
            
            # coef select UI
            output$`coef-ui` = renderUI({
                vars = colnames(emit$design)
                logjs(vars)
                selectInput(
                    session$ns("coef"), "Select the coefficient to test",
                    choices = vars, selected = vars[2]
                )
            })
            
            # event listener for coef input
            observeEvent(input$`coef-submit`, {
                emit$coef = input$coef
                emit$transform = input$transform
                emit$submitted = input$`coef-submit`
            })
            
            return(emit)
        },
        
        # call
        call = function(input, output, session, props){
            callModule(self$server, self$id, props)
        },
        
        # methods
        formulaValidator = function(x){
            f = tryCatch({
                as.formula(x)
            }, error = function(e){
                return(e)
            })
            if(is(f, "error")){
                return(FALSE)
            } else if(length(f) != 2 & as.character(f[[1]]) != "~"){
                return(FALSE)
            } else {
                return(TRUE)
            }
        },
        
        getVarsFromFormula = function(f){
            if(length(f) == 1){
                return(as.character(f))
            } else if(length(f) == 2){
                return(self$getVarsFromFormula(f[[2]]))
            } else if(length(f[[2]]) == 1){
                if(length(f[[3]]) == 1){
                    return(as.character(c(f[[2]], f[[3]])))
                } else {
                    return(c(as.character(f[[2]]), self$getVarsFromFormula(f[[3]])))
                }
            }  else {
                return(c(self$getVarsFromFormula(f[[2]]), self$getVarsFromFormula(f[[3]])))
            }
        },
        
        variableValidator = function(formula, varNames){
            vars = self$getVarsFromFormula(formula)
            vars = gsub("`", "", vars)
            return(vars[which(!(vars %in% varNames))])
        },
        
        getDesignMatrix = function(data, varNames, varTypes, formula){
            pdata = sample_table(data)
            pdata = as(pdata, "data.frame")
            for(i in seq_along(varNames)){
                changeType = switch(
                    varTypes[[i]],
                    "numeric" = as.numeric,
                    "factor" = factor
                )
                pdata[,varNames[[i]]] = changeType(pdata[,varNames[[i]]])
            }
            return(model.matrix(data = pdata, formula))
        }
    )
)