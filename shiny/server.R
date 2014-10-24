library(shiny)
library(datasets)
library(htmltools)

source('pair_func.R')

# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  
  teams <- reactive({
    teams <- readSpreadsheet(input$teams)
    return(teams)
  })

  tab <- reactive({
    df <- cleanTab(readSpreadsheet(input$df))
    tab <- tabSumm(df, amta = TRUE, round = as.numeric(input$round))
    return(tab)
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    tab <- tab()
    round <- as.numeric(input$round)
    coinTie <- input$coinTie
    tab$rank <- rankMT(dat = tab, 
                       crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', 
                       r = round, coinflip = coinTie)
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