library(shiny)
library(shinyjs)
ui = fluidPage(
  useShinyjs(),  # Set up shinyjs (this is just for nice visual effects)
  sidebarLayout(
    sidebarPanel(a(id = "imp_det", 
                   h3("Choose a dataset"), 
                   href = "#"),
                 shinyjs::hidden(div(
                   id = "details_import",
                   helpText(
                     h5("You can also upload your data!")
                   )
                 )),
                 selectInput(inputId = "dataset",
                             label = "",
                             choices = list("rock" = 1,
                                            "pressure" = 2,
                                            "cars" = 3,
                                            "I want to use my data!!" =4)),
                 
                 conditionalPanel(
                   condition = "input.dataset == '4'",
                   fileInput("example",
                             "", accept = c("csv"))
                 ),
                 actionButton("load", "Upload data"), 
                 conditionalPanel(                    
                   condition = "input.load >= '1'",  
                   uiOutput("var1"),                
                   uiOutput("var2"),                 
                   actionButton("select", "Select & Display") 
                 ),                              
                 
    ),
    
    mainPanel(
      plotOutput(
        "graph",
        click = clickOpts(id = "plot_click"), # when we click we select a point
        brush = brushOpts(id = "plot_brush") # when we highlight an area we select
      ),     # many rows
      fluidRow( # it displays on the same row multiple arguments
        column(4, # first column with the summary verbatim output
               verbatimTextOutput(
                 "summary"
               )),
        column(4,  # second column with another verbatim output for the points
               verbatimTextOutput(
                 "points"
               )
        ),
        column(4,
               verbatimTextOutput(
                 "brush"
               ))
      )
      
    ) # display output
  )
)
