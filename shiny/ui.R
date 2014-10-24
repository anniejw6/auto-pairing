library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  titlePanel("AMTA Auto-Pairing"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      
      h2("Link Tab"),
      helpText(a(href = "http://www.anniejw.com", targt = "_blank", 
                 "View instructions for setting up Google Sheets.")),
      
      textInput("teams", "Link to the teams sheet:",
                value = "https://docs.google.com/spreadsheets/d/1CU4WO8heN0LBXSURLOZQXopt0UV480Cmg7h8NuBXcRY/pubhtml?gid=0&single=true"),
      
      textInput("df", "Link to the running tab sheet:",
                value = "https://docs.google.com/spreadsheets/d/1CU4WO8heN0LBXSURLOZQXopt0UV480Cmg7h8NuBXcRY/pubhtml?gid=2080568749&single=true"),
      
      h2("Round Pairing"),
      selectInput("round", "Select which round you are pairing:", 
                  choices = 2:4),
      
      selectInput("coinTie", "Select the result of the coin-toss to determine tie-breakers:", 
                  choices = c('Heads', 'Tails')),
      
      helpText("Note: This only needs to be performed once per tournament",
               "to determine what happens in case of ties.",
               style = "font-size: 8pt"),
      
      selectInput("coinR3", "Select the result of the coin-toss to determine round 3 sides:", 
                  choices = c('Heads', 'Tails')),
      
      helpText("Note: This only needs to be performed before Round 3.",
               style = "font-size: 8pt"),
      
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("rock", "pressure", "cars"),
                  selected = "cars"),
      
      numericInput("obs", "Number of observations to view:", 3),
      
      submitButton("Generate Pairings")
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      h4("Observations"),
      htmlOutput("view")
    )
  )
))