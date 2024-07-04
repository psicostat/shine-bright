library(shiny)
library(shinyjs)
server = function(input, output){
  values <- reactiveValues()
  dataInput <- reactive({
    if(input$dataset == 1){
      data <- rock
    } else if (input$dataset == 2 ){
      data <- pressure
    } else if (input$dataset == 3) {
      data <- cars
    } else if (input$dataset == 4) {
      data <- read.csv(input$example$datapath)
    }
  })
  observeEvent(input$load, {
    values$data <- data.frame(dataInput())
    # check the characters
    if (any(sapply(values$data, is.character)) == TRUE) {
      values$data[, sapply(values$data, is.character) == T] = lapply(values$data[, sapply(values$data, is.character) == T], as.factor)
    } else {
      values$data = values$data
    }
  })
  shinyjs::onclick("imp_det",  # here the nice visual effect
                   shinyjs::toggle(id = "details_import", anim = TRUE))
  output$var1 <- renderUI({    
    nam <- colnames(values$data) 
    selectInput("var1", label = "Select x:", 
                choices = c(nam), multiple = F,
                selected = nam[1])
  })
  
  output$var2 <- renderUI({
    nam2 <- colnames(values$data) 
    selectInput("var2", label = "Select y:",
                choices = c(nam2), multiple = F,
                selected = nam2[1])
  })
  
  newdata <- observeEvent(input$select, 
                          { # wait for you to decide before acting
                            # Besides, you're creating a new (smaller) object
                            values$df <- values$data[c(input$var1, input$var2)]
                          })
  output$graph <- renderPlot({
    validate(
      need(input$select > 0, "Waiting for data") 
    )                                        # load to select
    
    df = values$df
    plot(df[, c(1:2)]) # use it normally
    
    
  })
  
  output$summary <- renderPrint({
    validate(
      need(input$select > 0, "Waiting for data")
    )
    df <- values$df # same
    summary(df[, c(1:2)])
    
  })
  
  output$points <- renderPrint({
    df <- values$df # store the data frame in an object
    pointID <- nearPoints(df, # the data frame
                          input$plot_click, # the command for a reaction
                          xvar = names(df)[colnames(df) == input$var1], # xvar of the graph
                          yvar = names(df)[colnames(df) == input$var2], # yvar of the graph,
                          addDist = FALSE)
    validate(
      need(nrow(pointID) != 0, "Click on a point") # Waiting message
    )
    pointID
  })
  
  output$brush <- renderPrint({
    df <- values$df # store the data frame in an object
    brushID <- brushedPoints(df,# the  data frame
                             input$plot_brush, # the command for a reaction
                             xvar = names(df)[colnames(df) == input$var1], # xvar of the graph
                             yvar = names(df)[colnames(df) == input$var2], # yvar of the graph
    )
    validate(
      need(nrow(brushID) != 0, "Highlight Area") # Waiting message
    )
    brushID
  })
  
}