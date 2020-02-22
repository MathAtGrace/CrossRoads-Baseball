library(rvest)
library(stringr)
library(tidyverse)

#Wasn't sure if this needs to be done every time?
load("Data/stats_by_type.Rdata")
Batting <- baseball[["Batting"]]
Pitching <- baseball[["Pitching"]]
Fielding <- baseball[["Fielding"]]
teams <- names(Batting)

#General info
teams <- c("BC","GOC","GRC","HU","INWU","MAR","MVNU","SAU","SFIN","TAYL")
names(teams) <- c(1629, 1678, 1679, 1688, 1694, 1717, 1736, 1780, 1805, 1784) 

#get schedules from dakstats
schedule_urls <- paste0("http://www.dakstats.com/WebSync/Pages/Team/TeamSchedule.aspx?association=10&sg=MBA&sea=NAIMBA_2019&team=",names(teams))
View(schedule_urls)

#function to get the schedule of a team
get_schedule <- function(schedule_urls) {
  bpf <- schedule_urls %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[36]] %>%
    html_table(fill = TRUE)
}

#p_effects_urls <- teams %>%
  #lapply(get_schedule)
#names(p_effects_urls) <- teams

schedules <- lapply(schedule_urls, get_schedule)
names(schedules) <- teams
View(schedules)

#A function to clean up the tables and show only conference schedule
conference <- function(x){
  #Gets rid of rows containing only NA
  schedules[[x]] <- schedules[[x]][!apply(is.na(schedules[[x]]) | schedules[[x]] == "", 1, all),]
  #Rename columns
  names(schedules[[x]])[1] <- "Date"
  names(schedules[[x]])[2] <- "Opponent"
  names(schedules[[x]])[3] <- "Location"
  names(schedules[[x]])[4] <- "Score"
  names(schedules[[x]])[5] <- "Decision"
  #Grab only the conference games
  CL_schedule <- schedules[[x]] %>%
    filter(str_detect(Opponent, "\\*"))
  #remove column with only NA at the end
  CL_schedule <- CL_schedule[-6]
  #output only the CL schedules
  schedules[[x]] <- CL_schedule
}

schedules <- lapply(teams, conference)
names(schedules) <- teams


#A function to write the data to csv files
writeit <- function(x) {
  write.csv(schedules[[x]], file = paste0("Data/Schedules", "/", x, ".csv"))
}

#Write to csv
lapply(teams, writeit)
