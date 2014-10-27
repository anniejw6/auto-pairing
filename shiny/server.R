library(shiny)
library(datasets)
library(htmltools)

source('pair_func.R')

# Define server logic required to summarize and view the 
# selected dataset
shinyServer(function(input, output) {
  
  observe({
    if (input$submit == 0)
      return()
    
    isolate({
      
      ##### Set Inputs ####
      amta <- T
      round <- as.numeric(input$round)
      coinTie <- tolower(input$coinTie)
      coinR3  <- tolower(input$coinR3)
      
      # Read inputs and save values to database here
      teams <- reactive({
        teams <- readSpreadsheet(input$teams)
        return(teams)
      })
      
      df <- reactive({
        cleanTab(readSpreadsheet(input$df))
      })
      
      tab <- reactive({
        tab <- tabSumm(df(), amta = amta, round = round)
        tab$rank <- rankMT(dat = tab, 
                           crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', 
                           r = round, coinflip = coinTie)
        return(tab)
      })
      
      # Define impermissible
      impermiss <- reactive({
        rbind(sameSchool(teams()),
              pastOpp(df(), round))
      })
      
      ##### Pair Teams #####
      pair <- tab()
      
      #if round is side-constrained:
      if(round %in% c(2,4)){
        
        #rank sides separately
        pair$side <- ifelse(pair$side == 'P', 'D', 'P')  
        pair <- ddply(pair, .(side), mutate, rank = rank(rank))
        
        #pair highest versus highest
        pair <- pair[order(pair$rank, pair$side), ]
        
      } else {
        
        #rank teams together
        pair <- pair[order(pair$rank), ]
        
        #pair 1 vs 2, 3 vs 4, etc
        if(coinR3 == 'heads'){
          pair$side <- rep(c('P', 'D'))
        } else {
          pair$side <- rep(c('D', 'P'))
        }
        
        
      }
      
      pair$trial <- rep(1:(nrow(pair)/2), each = 2)
      
      ##### Find Impermissibles #####
      # Find impermissibles
      pair$impermiss <- findImpermiss(pair, impermiss())
      
      # Set value to store swaps
      swaps <- data.frame(Team1 = NA, Team2 = NA, final = NA)
      
      ##### Initial Pairings #####
      output$summary <- renderText({
        printTab(addColor(pairPretty(pair, amta = amta, impermiss = T)))
      })
      
      ##### Resolve Impermissibles ######
      res <- NULL
      
      # If there are no impermissibles,
      if(sum(pair$impermiss) == 0){
        
        res <- c(res, '<p>No impermissibles!</p>')
        
      } else {
        
        pair$newRank <- pair$rank
        
        # resolve impermissibles
        while (sum(pair$impermiss) > 0){
          
          # Print pairings at start
          res <- c(res, '<p>Current List of Pairings</p>')
          res <- c(res, printTab(addColor(pairPretty(pair, amta = amta))))
          
          # set trial_x = highest trial with impermissible
          trial_x <- pair[pair$trial == pair$trial[min(which(pair$impermiss == T))], ]
          
          # Compare swap distances based on WPB, PB, and PD
          possSwaps <- compareDist(x = trial_x, all = pair, round = round, amta = F)  
          
          repeat{
            
            # Set proposed_swap = minimum distance swap
            proposedSwap <- possSwaps[1, ]
            
            # proposed Swap
            res <- c(res, '<p>Proposed Swap</p>')
            res <- c(res, printTab(proposedSwap))
            
            # If it's allowed
            if(!paste(swapList(newSwap = proposedSwap, oldSwap = trial_x), collapse = '') 
               %in% swaps$final){
              break # Move on
            }
            
            # If it's not allowed, remove proposed_swap from possible
            res <- c(res, '<p>Proposed swap is not possible!</p>')
            possSwaps <- possSwaps[-1, ]
          }
          
          #  make proposed_swap
          pair <- makeSwap(newSwap = proposedSwap, old = trial_x, dat = pair)
          
          #  insert proposed_swap in SWAP
          swaps <- insertSwap(newSwap = proposedSwap, oldSwap = trial_x, dat = swaps)
          
          #  set n = number of impermissibles
          pair$impermiss <- findImpermiss(pair, impermiss())
        }
      }
      
      #### Print Impermissible #####
      output$view <- renderText({
        res
      })
      
      #### Print Final #####
      output$final <- renderText({
        printTab(addColor(pairPretty(pair, amta = amta, impermiss = T)))
      })
    })
  })
  
})