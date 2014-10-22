library(shiny)
library(datasets)
library(htmltools)

source('pair_func.R')

# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    dataset <- datasetInput()
    print(input$dataset)
  })
  
  # Show the first "n" observations
  output$view <- renderText({
#    renderTable({
#    head(datasetInput(), n = input$obs)
    res <- printTab(head(datasetInput()))
    res <- c(res, "<p> hello </p>")
    res <- c(res, printTab(head(datasetInput())))
    res
 # })
  })
})