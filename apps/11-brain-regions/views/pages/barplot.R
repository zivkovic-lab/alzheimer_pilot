PageBarplot = R6Class(
    "PageBarplot",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "page_bar",
        states = reactiveValues(
            nrow = 0,
            ncol = 0,
            glycans = list(),
            regions = list(),
            glycans_inputs = list(),
            regions_inputs = list(),
            glycans_input_listeners = list(),
            regions_input_listeners = list()
        ),
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                uiOutput(ns("row_inputs")),
                uiOutput(ns("col_inputs")),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = NULL, 
                            radioButtons(
                                ns("data_type"), label = NULL,
                                choices = c("rel_abund", "abs_abund"),
                                inline = TRUE
                            ),
                            jqui_resizable(
                                plotlyOutput(ns("plot"))
                            )
                        )
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$row_inputs = renderUI({
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = NULL,
                            column(
                                width = 12,
                                tags$h4("Glycans", class="d-inline-block")
                            ),
                            self$states$glycans_inputs,
                            column(
                                width = 2,
                                actionButton(
                                    session$ns("add_row"),
                                    label = NULL,
                                    icon = icon("plus"),
                                    class="btn-primary btn-xs",
                                    inline = TRUE
                                ),
                                actionButton(
                                    session$ns("del_row"),
                                    label = NULL,
                                    icon = icon("trash-alt"),
                                    class="btn-danger btn-xs",
                                    inline = TRUE
                                )
                            )
                        )
                    )
                )
            })
            output$col_inputs = renderUI({
                fluidRow(
                    column(
                        width = 12,
                        box(
                            width = NULL,
                            column(
                                width = 12,
                                tags$h4("Brain Regions", class="d-inline-block")
                            ),
                            self$states$regions_inputs,
                            column(
                                width = 2,
                                actionButton(
                                    session$ns("add_col"),
                                    label = NULL,
                                    icon = icon("plus"),
                                    class="btn-primary btn-xs",
                                    inline = TRUE
                                ),
                                actionButton(
                                    session$ns("del_col"),
                                    label = NULL,
                                    icon = icon("trash-alt"),
                                    class="btn-danger btn-xs",
                                    inline = TRUE
                                )
                            )
                        )
                    )
                )
            })
            observeEvent(input$add_row, {
                self$states$nrow = self$states$nrow + 1
                nrow = self$states$nrow
                # append a select input for glycan
                self$states$glycans_inputs = lapply(seq_len(nrow), function(i){
                    selected = if(i < nrow) self$states$glycans[[i]] else NULL
                    column(
                        width = 2,
                        selectInput(
                            session$ns(paste0("glycan-", i)),
                            "select a glycan:",
                            choices = featureNames(.DATA$data$rel_abund),
                            selected = selected
                        )
                    )
                })
                # append a select input listener
                inputId = paste0("glycan-", nrow)
                self$states$glycans_input_listeners[[nrow]] = observeEvent(
                    input[[inputId]], {
                        self$states$glycans[[nrow]] = input[[inputId]]
                })
            })
            
            observeEvent(input$del_row, {
                nrow = self$states$nrow
                self$states$nrow = nrow - 1
                self$states$glycans = self$states$glycans[-nrow]
                # append a select input for glycan
                self$states$glycans_inputs = lapply(seq_len(nrow - 1), function(i){
                    selected = self$states$glycans[[i]]
                    column(
                        width = 2,
                        selectInput(
                            session$ns(paste0("glycan-", i)),
                            "select a glycan:",
                            choices = featureNames(.DATA$data$rel_abund),
                            selected = selected
                        )
                    )
                })
                # append a select input listener
                self$states$glycans_input_listeners = self$states$glycans_input_listeners[-nrow]
            })
            
            observeEvent(input$add_col, {
                self$states$ncol = self$states$ncol + 1
                ncol = self$states$ncol
                # append a select input for regions
                self$states$regions_inputs = lapply(seq_len(ncol), function(i){
                    selected = if(i < ncol) self$states$regions[[i]] else NULL
                    column(
                        width = 2,
                        selectInput(
                            session$ns(paste0("region-", i)),
                            "selected a region:",
                            choices = unique(.DATA$data$rel_abund$pdata$region),
                            selected = selected
                        )
                    )
                })
                # append a select input listener
                inputId = paste0("region-", ncol)
                self$states$regions_input_listeners[[ncol]] = observeEvent(
                    input[[inputId]], {
                        self$states$regions[[ncol]] = input[[inputId]]
                })
            })
            
            observeEvent(input$del_col, {
                ncol = self$states$ncol
                self$states$ncol = ncol - 1
                self$states$regions = self$states$regions[-ncol]
                # append a select input for regions
                self$states$regions_inputs = lapply(seq_len(ncol-1), function(i){
                    selected = self$states$regions[[i]]
                    column(
                        width = 2,
                        selectInput(
                            session$ns(paste0("region-", i)),
                            "selected a region:",
                            choices = unique(.DATA$data$rel_abund$pdata$region),
                            selected = selected
                        )
                    )
                })
                self$states$regions_input_listeners = self$states$regions_input_listeners[-ncol]
            })
            
            output$plot = renderPlotly({
                .DATA$plot_glycan_barplots(
                    data_type = input$data_type,
                    glycans = self$states$glycans,
                    regions = self$states$regions
                )
            })
        }
    )
)