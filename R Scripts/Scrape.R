library(rvest)
#require(tidyverse) do I need this?

#We'll use Kara's names for the teams
teams <- c("BC","GOC","GRC","HU","INWU","MAR","MVNU","SAU","SFIN","TAYL")
#R lets us rename the indices of vectors.
names(teams) <- c(1629, 1678, 1679, 1688, 1694, 1717, 1736, 1780, 1805, 1784) 

#Create the url's that we will scrape the data from
urls <- paste0("http://www.dakstats.com/WebSync/Pages/Team/IndividualStats.aspx?association=10&sg=MBA&conference=NAIMBA1_CROSS&team=",names(teams),"&sea=NAIMBA_2019")

#A function for getting the the batting tables we need from each webpage
get_batting <- function(url) {
  bpf <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[37]] %>%
    html_table(fill = TRUE)
}

#A function for getting the the pitching tables we need from each webpage
get_pitching <- function(url) {
  bpf <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[38]] %>%
    html_table(fill = TRUE)
}

#A function for getting the the fielding tables we need from each webpage
get_fielding <- function(url) {
  bpf <- url %>%
    read_html() %>%
    html_nodes("table") %>%
    .[[39]] %>%
    html_table(fill = TRUE)
}

#lapply does the same function to each url.
batting <- lapply(urls, get_batting)
names(batting) <- teams
pitching <- lapply(urls, get_pitching)
names(pitching) <- teams
fielding <- lapply(urls, get_fielding)
names(fielding) <- teams

baseball <- list(batting, pitching, fielding)
names(baseball) <- c("Batting", "Pitching", "Fielding")

#A function to write the data to csv files
writeit <- function(x) {
  #Batting
  write.csv(batting[[x]], file = paste0("Data/Batting", "/", x, ".csv"))
  #Pitching
  write.csv(pitching[[x]], file = paste0("Data/Pitching", "/", x, ".csv"))
  #Fielding
  write.csv(fielding[[x]], file = paste0("Data/Fielding", "/", x, ".csv"))
}

#Write to csv
lapply(teams, writeit)

#Save to an Rdata file
save(baseball, file = "Data/stats_by_type.Rdata")
