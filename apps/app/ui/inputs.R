groupSelector = function(type){
    selectInput(inputId = paste0(type, ".group"),
                label = "Select a Comparison Group",
                choices = c("AD   E3/E3", "AD   E3/E4"),
                selected = "AD   E3/E3")
}

methodSelector = function(type, choices){
    selectInput(inputId = paste0(type, ".method"), 
                label = "Select a Correlation Method",
                choices = choices,
                selected = choices[1])
}

output$VarsInput = renderUI({
    type = substr(input$sidebar, 1, 3)
    
    if(type == "glc"){
        choices1 = names(data$data$glc)
        choices2 = names(data$corr$glc$fct$peptide)
        tagList(
            selectInput("glc.level", "Peptide or Glycans?",
                        choices = choices1, selected = choices1[1]),
            selectInput("glc.method", "Select a Correlation Method",
                        choices = choices2, selected = choices2[1]),
            groupSelector(type)
        )
    } else if(type == "fct"){
        tagList(
            groupSelector(type)
        )
    }
})