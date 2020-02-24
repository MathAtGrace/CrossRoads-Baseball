library(tidyverse)
library(elo)

#place to store each team's elo rating
ratings <- data.frame(team = teams)
ratings <- ratings %>%
  mutate(elo = 1500)
View(ratings)

#function that adds a binary variable
#1 if home wins/away loses and 0 if home loses/away wins
add_elo_score <- function(x){
  schedules[[x]] <- schedules[[x]] %>%
    mutate(result = ifelse(grepl("W",Decision,fixed=TRUE)&grepl("H",Location,fixed=TRUE),1,ifelse(grepl("A",Location,fixed=TRUE)&grepl("L",Decision, fixed=TRUE),1,0)))
  #change opponent name to match abbreviations
  schedules[[x]] <- schedules[[x]] %>%
    mutate(Opp = ifelse(grepl("Bethel (Ind.) *",Opponent,fixed=TRUE),teams[1],ifelse(grepl("Goshen (Ind.) *",Opponent,fixed=TRUE),teams[2],ifelse(grepl("Grace (Ind.) *",Opponent,fixed=TRUE),teams[3],ifelse(grepl("Huntington (Ind.) *",Opponent,fixed=TRUE),teams[4],ifelse(grepl("Indiana Wesleyan *",Opponent,fixed=TRUE),teams[5],ifelse(grepl("Marian (Ind.) *",Opponent,fixed=TRUE),teams[6],ifelse(grepl("Mount Vernon Nazarene (Ohio) *",Opponent,fixed=TRUE),teams[7],ifelse(grepl("Spring Arbor (Mich.) *",Opponent,fixed=TRUE),teams[8],ifelse(grepl("St. Francis (Ind.) *",Opponent,fixed=TRUE),teams[9],teams[10]))))))))))
}

schedules <- lapply(teams, add_elo_score)
names(schedules) <- teams

#creating a master schedule with all games
AllGames <- do.call("rbind",schedules)
HomeGames <- AllGames %>%
  filter(str_detect(Location, "H"))
View(HomeGames)


#Fix extra innings issue
Opp <- HomeGames[["Opp_Score"]]

for (x in seq(length(Opp))){
  if (grepl("(", Opp[x], fixed = TRUE) == TRUE){
     Opp[x] <- as.integer(substr(Opp[x], 1, nchar(Opp[x])-4))
    
    }
}

HomeGames[["Opp_Score"]] <- Opp



#for loop modified from the original version found here: https://edomt.github.io/Elo-R-WorldCup/
for (i in seq(1, nrow(HomeGames))) {
  
  match <- HomeGames[i,]
  
  View(match)
  
  # Pre-match ratings
  teamA_elo <- subset(ratings, team == match$Team)$elo
  teamB_elo <- subset(ratings, team == match$Opp)$elo
  
  #print(teamA_elo)
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)
  #print(new_elo)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  ratings <- ratings %>%
    mutate(elo = if_else(team == match$Team, teamA_new_elo,
                         if_else(team == match$Opp, teamB_new_elo, elo)))
}


#schedules <- lapply(teams, update_elo)
#names(schedules) <- teams
View(ratings)

