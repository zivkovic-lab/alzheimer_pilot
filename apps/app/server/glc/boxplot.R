glc_limma_table = reactive({
    data$lm$glc[[input$glc.level]][[input$glc.group]]
})

glc_limma = reactive({
    glc_limma_table()%>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$glc_limma = renderDT(
    glc_limma(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

glc_boxplot_selector = reactive({
    rownames(glc_limma())[input$glc_limma_rows_selected]
})

output$glc_boxplot = renderPlotly({
    mset = data$data$glc[[input$glc.level]]
    p = plot_boxplot(mset, 
                     x = "Group", 
                     feature = glc_boxplot_selector()) +
        labs(x = "", y = "")
    ggplotly(p)
})