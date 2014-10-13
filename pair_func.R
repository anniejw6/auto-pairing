library(XML)
library(httr)
library(plyr)
library(reshape)
library(knitr)
library(xtable)

# Prints pretty things
printTab <- function(x){
  x <- xtable(x)
  print(x, type = 'html', include.rownames = F, 
        sanitize.text.function = force, 
        sanitize.rownames.function = NULL, 
        sanitize.colnames.function = NULL)
}

# Adds color to impermissibles
addColor <- function(x = pair){
  x[x$impermiss == T, ] <- as.data.frame(lapply(x[x$impermiss == T,],
                                                FUN = function(x) {paste0('<span style="color:red">', x, '</span>')}),
                                         stringsAsFactors = F)
  return(x)
}

# Read from spreadsheet
readSpreadsheet <- function(url, sheet = 1){
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=FALSE, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1] 
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}

cleanTab <- function(dat, amta){
  
  # Clean up columns
  del <- c(paste0('S.', 1:3), '','S', 'CURRENT_WPB', 'CURRENT_RANK', 
           'CURRENT_BAL', 'CURRENT_CS', 'CURRENT_PD')
  df <- dat[, !colnames(dat) %in% del]
  df <- subset(df, TEAM != '')
  
  # Deal with NAs
  df[df == '#N/A'] <- NA
  df[df == 'NA'] <- NA
  
  # Get rid of factors
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  
  return(df)
    
  }

tabSumm <- function(df, amta){
  if(amta == F){
    
    tab <- data.frame(
      team = df$TEAM,
      side = df[[paste0('R', round - 1, '_SIDE')]],
      crit1  = as.numeric(df[[paste0('R', round - 1, '_RT_WPB')]]),
      crit2   = as.numeric(df[[paste0('R', round - 1, '_RT_PB')]]),
      crit3   = as.numeric(df[[paste0('R', round - 1, '_RT_PD')]]),
      stringsAsFactors = F
    )
    
  } else {
    tab <- data.frame(
      team = df$TEAM,
      side = df[[paste0('R', round - 1, '_SIDE')]],
      crit1  = as.numeric(df[[paste0('R', round - 1, '_RT_BAL')]]),
      crit2   = as.numeric(df[[paste0('R', round - 1, '_RT_CS')]]),
      crit3   = as.numeric(df[[paste0('R', round - 1, '_RT_PD')]]),
      stringsAsFactors = F
    )
  }
  return(tab)
}

# Create data-frame of impermissible based on same school
sameSchool <- function(df){
  teams <- subset(df, University %in% University[duplicated(University)])
  x <- ddply(teams, .(University), function(x){
    res <- t(combn(x[['Team Name']], 2))
    oth <- res[, c(2, 1)]
    res <- data.frame(rbind(res, oth))
    return(res)
  })
  x$University <- NULL
  colnames(x) <- c('Team1', 'Team2')
  return(x)
}

# Create data-frame of impermissible based on past opponents
pastOpp <- function(df, round){
  
  # Opponents up to that round
  impermiss <- df[, c('TEAM', paste0('R',1:(round - 1),'_OPP'))]
  impermiss <- melt(impermiss, id = 'TEAM')
  
  # Get rid of random variable
  impermiss$variable <- NULL
  
  # Subset to rounds that have been paired
  impermiss <- subset(impermiss, !is.na(value))
  
  # Clean-up
  colnames(impermiss) <- c('Team1', 'Team2')
  
  return(impermiss)
}


### Make Pairs look pretty ###
pairPretty <- function(dat = pair, amta = amta, impermiss = T){
  
  # Order by Trial
  dat <- dat[order(dat$trial, dat$side), ]
  
  # Set column names
  if(amta == T){
    crits <- c('Ballots', 'CS', 'PD')
  } else {
    crits <- c('WPB', 'PB', 'PD')
  }
  
  x <- colnames(dat)
  colnames(dat)[c(grep('crit1', x), 
                  grep('crit2', x), 
                  grep('crit3', x))] <- crits
  
  
  # Stack P and D
  p <- subset(dat, side == 'P')
  colnames(p) <- paste0('p_', colnames(p))
  d <- subset(dat, side == 'D')
  colnames(d) <- paste0('d_', colnames(d))
  x <- cbind(p, d)
  
  # Clean up Columns
  x <- x[, !grepl('side', colnames(x))]
  x <- x[, c('p_trial', 'p_team', 'd_team', 
             paste0('p_', crits), 'p_rank', 
             paste0('d_', crits), 'd_rank')]
  colnames(x)[1] <- 'trial'
  
  # Add Impermissibles
  if(impermiss == T){
    x$impermiss <- p$p_impermiss
  }
  
  # Round 
  i <- which(colnames(x) %in% c(paste0('p_', crits), paste0('d_', crits)))
  x[i] <- lapply(x[i], function(x) round(x, 3))
  
  return(x)
}

# rank teams by WPB, PB, PD, coin-flip
rankMT <- function(dat = tab, crit1 = 'crit1', crit2 = 'crit2', crit3 = 'crit3', r = round, coinflip = 'heads'){
  
  if(coinflip == 'heads'){
    coinflip <- rank(tab$team)
    print('In case of ties, larger team number gets higher rank.')
  }else {
    coinflip <- -rank(tab$team)
    print('In case of ties, lower team number gets higher rank.')
  }
  
  dat[is.na(dat)] <- 0
  crit1 <- dat[[crit1]]
  crit2 <- dat[[crit2]]
  crit3 <- dat[[crit3]]
  
  return(rank(-1*as.numeric(
    interaction(crit1, crit2, crit3, coinflip,
                drop = TRUE, lex.order = TRUE)))) 
}

# Calculate distance by WPB, PB, PD
# Returns 
compareDist <- function(x = trial_x, all = pair, round = round, amta = F){

  # Minor Cleaning
  all[is.na(all)] <- 0
  x[is.na(x)] <- 0

  # Create all possible matches 
  calcDist <- function(x, all, sideT, roundNum = round){
    
    x <- subset(x, side == sideT)
    
    if(roundNum %% 2 == 0){
      all <- subset(all, side == sideT)
    }
    
    if(sideT == 'D'){
      cat <-  'Keep P, Swap D'
    } else {
      cat <- 'Keep D, Swap P'
    }
    return(
      data.frame(
        dist_crit1 = abs(all$crit1 - x$crit1),
        dist_crit2 = abs(all$crit2 - x$crit2),
        dist_crit3 = abs(all$crit3 - x$crit3),
        dist_rank = abs(all$newRank - x$newRank),
        cat = cat
        )
    )
  }
  
  if(round %% 2 == 0){ # If round is side-constrained
    
    # Compare against teams of the same side
    dSwap <- data.frame(p = x$team[x$side == 'P'],
                        d = all$team[all$side == 'D'])
    dSwap <- cbind(dSwap, calcDist(x = x, all = all, sideT = 'D'))
    
    pSwap <- data.frame(p = all$team[all$side == 'P'],
                        d = x$team[x$side == 'D'])
    pSwap <- cbind(pSwap, calcDist(x = x, all = all, sideT = 'P'))
    
  } else {
    # Compare against all teams
    dSwap <- data.frame(p = x$team[x$side == 'D'],
                        d = all$team)
    dSwap <- cbind(dSwap, calcDist(x = x, all = all, sideT = 'D'))
    
    pSwap <- data.frame(p = all$team,
                        d = x$team[x$side == 'D'])
    pSwap <- cbind(pSwap, calcDist(x = x, all = all, sideT = 'P'))
    
  }
  res <- unique(rbind(dSwap, pSwap))
  
  # Get rid of factors
  i <- sapply(res, is.factor)
  res[i] <- lapply(res[i], as.character)
  
  # Get rid of matches where you're swapping the original ones,
  # or where you're matching against the same team.
  res <- subset(res, p != d)
  res <- subset(res, !paste0(res$p, res$d) %in% c(paste0(x$team, collapse =''), 
                                          paste0(rev(x$team), collapse = '')))
  
  # Sort by distance
  res <- res[order(res$dist_rank, res$dist_crit1, res$dist_crit2, res$dist_crit3), ]
  res <- subset(res, dist_rank > 0) # This is the no-swap option

  return(res)
}

# Find Impermissibles
findImpermiss <- function(dat = pair, impermissibles = impermiss){
  
  notOkay <- paste0(impermissibles[, 1], impermissibles[, 2])
  
  res <- rep(F, nrow(dat))
  
  for(i in 1:nrow(dat)){
    
    proposedPair <- paste(pair$team[pair$trial == pair$trial[i]], collapse = '')
      
    if(proposedPair %in% notOkay){
      res[i] <- T
    }
    
  }
  return(res)
}

# Make proposed swap
makeSwap <- function(newSwap = proposedSwap, old = trial_x, dat = pair){
  
  swap <- function(swapSide = 'P', dat = dat){
    
    # Get row numbers
    oldD <- which(dat$team == old$team[old$side == swapSide])
    newD <- which(dat$team == newSwap[[tolower(swapSide)]])
    
    #writeLines('\nOriginal values\n')
    #printTab(xtable(dat[c(oldD, newD), ]))
    
    # Make the swap
    dat$trial[c(oldD, newD)] <- dat$trial[rev(c(oldD, newD))]
    dat$side[c(oldD, newD)] <-  dat$side[rev(c(oldD, newD))]
    dat$newRank[c(oldD, newD)] <-  dat$newRank[rev(c(oldD, newD))]
    
    #writeLines('\nNew values\n')
    #printTab(xtable(dat[c(oldD, newD), ]))

    return(dat)
  }
  
  if(newSwap$cat == 'Keep D, Swap P'){
    #writeLines(paste('\nSwapping Pros team', old$P_team, 'for', newSwap$p))
    side = 'P'
  } else {
    #writeLines(paste('\nSwapping Def team', old$D_team, 'for', newSwap$d))
    side = 'D'
  }

  return(swap(swapSide = side, dat = dat))
  
}

# Generate Swap
swapList <- function(newSwap = proposedSwap, oldSwap = trial_x){
  
  if(newSwap$cat == 'Keep P, Swap D'){
    neW <- c(team1 = newSwap$d,
                      team2 = trial_x$team[trial_x$side == 'D'])
  } else {
    neW <- c(team1 = newSwap$p,
                      team2 = trial_x$team[trial_x$side == 'P'])
  }
  return(neW)
}

# Insert Swaps
insertSwap <- function(newSwap = proposedSwap, oldSwap = trial_x, dat = swaps){

  x <- swapList(newSwap = proposedSwap, oldSwap = trial_x)
  # Create out combinations
  news <- data.frame(Team1 = x,
                     Team2 = rev(x),
                     final = NA,
                     stringsAsFactors = F)
  # Add to existing
  dat <- rbind(dat, news)
  # Get rid of NAs
  dat <- subset(dat, !is.na(Team1))
  # Create concatenation
  dat$final <- apply(dat, 1, function(x){
    paste0(x[1], x[2])
  })
  return(dat)
}