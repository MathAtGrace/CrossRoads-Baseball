library(rvest)
require(tidyverse)

load("Results2.Rdata")
batting <- results[["Batting"]]
pitching <- results[["Pitching"]]
fielding <- results[["Fielding"]]
teams <- names(batting)

bat_stats <- function(y){
  x <- batting[[y]]
  #Runs Created
  x$RC <- (x$H + x$BB)*x$TB/(x$AB + x$BB)
  #Caught Stealing
  x$CS <- x$SBA - x$SB
  #OPS
  x$OPS <- x$OB + x$SLG
  #POP
  x$POP <- x$OPS + x$AVG
  #Total Average
  x$TA <- (x$TB + x$BB + x$HBP + x$SB)/(x$AB - x$H + x$SH + x$SF + x$CS + x$GDP)
  #Batting Average on Balls in Play
  x$BABIP <- (x$H - x$HR)/(x$AB - x$SO - x$HR + x$SF)
  #Hoban Efficiency Quotient - offense
  x$HEQO <- x$TB + x$R + x$RBI + x$SB + (0.5*x$BB)
  x
}

batting <- teams %>%
  lapply(bat_stats)
names(batting) <- teams

#Loop
for(i in seq(length(teamid))){
  
  #Read the webpage
  #a <- paste("web_",abbr[[1]],sep="")
  #print(a)
  
  a <- read_html(paste("http://www.dakstats.com/WebSync/Pages/Team/IndividualStats.aspx?association=10&sg=MBA&conference=NAIMBA1_CROSS&team=",teamid[[i]],"&sea=NAIMBA_2019",sep=""))
  
  #paste(web_,abbr[[1]],sep="")
  #Get the three tables
  tbls_ls <- a %>%
    html_nodes("table") %>%
    .[37:39] %>%
    html_table(fill = TRUE)
  
  #Create the data frames
  Batting <- data.frame(tbls_ls[[1]])
  Pitching <- data.frame(tbls_ls[[2]])
  Fielding <- data.frame(tbls_ls[[3]])
  
  
  #FIP - need constant (using league average)
  #Pitching$FIP <- (13*Pitching$HR + 3 * (Pitching$BB + Pitching$HBP) - 2*Pitching$SO)/Pitching$IP
  
  #Runs Created
  x$RC <- (x$H + x$BB)*x$TB/(x$AB + x$BB)
  
  #Caught Stealing
  x$CS <- x$SBA - x$SB
  
  #OPS
  x$OPS <- x$OB. + x$SLG.
  
  #POP
  x$POP <- x$OPS + x$AVG
  
  #Total Average
  x$TA <- (x$TB + x$BB + x$HBP + x$SB)/(x$AB - x$H + x$SH + x$SF + x$CS + x$GDP)
  
  #Batting Average on Balls in Play
  x$BABIP <- (x$H - x$HR)/(x$AB - x$SO - x$HR + x$SF)
  
  #Hoban Efficiency Quotient - offense
  x$HEQO <- x$TB + x$R + x$RBI + x$SB + (0.5*x$BB)
  
  #Pythagorean Record (using x = 1.82)
  #We are using "=" because they're just numbers, not columns
  RS = x$R[x$Batting == "Total:"]
  RA = x$R[x$Batting == "Opponents:"]
  PR = RS^1.82/(RS^1.82 + RA^1.82)
  
  #Save the data frames as csv files
  write.csv(Batting, paste(abbr[[i]],"/Batting.csv",sep=""))
  write.csv(Pitching, paste(abbr[[i]],"/Pitching.csv",sep=""))
  write.csv(Fielding, paste(abbr[[i]],"/Fielding.csv",sep=""))
  
  #Defining variables from the csv files
  assign(paste(abbr[[i]],"Batting", sep=""),read.csv(paste(abbr[[i]],"/Batting.csv",sep="")))
  assign(paste(abbr[[i]],"Pitching",sep=""),read.csv(paste(abbr[[i]],"/Pitching.csv",sep="")))
  assign(paste(abbr[[i]],"Fielding",sep=""),read.csv(paste(abbr[[i]],"/Fielding.csv",sep="")))
}