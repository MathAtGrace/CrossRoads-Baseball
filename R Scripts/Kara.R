library(rvest)
#require(tidyverse)

load("Data/stats_by_type.Rdata")
batting <- baseball[["Batting"]]
pitching <- baseball[["Pitching"]]
fielding <- baseball[["Fielding"]]
teams <- names(batting)

pitch_stats <- function(y){
  x <- pitching[[y]]
  #FIP (still need to add constant)
  x$FIP <- ((13*x$HR)+3*(x$BB+x$HBP)-(2*x$SO))/(x$IP)
  x
}

pitching <- teams %>%
  lapply(pitch_stats)
names(pitching) <- teams
