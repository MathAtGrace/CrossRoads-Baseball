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
  schedules_19[[x]] <- schedules_19[[x]] %>%
    mutate(result = ifelse(grepl("W",Decision,fixed=TRUE)&grepl("H",Location,fixed=TRUE),1,ifelse(grepl("A",Location,fixed=TRUE)&grepl("L",Decision, fixed=TRUE),1,0)))
  #change opponent name to match abbreviations
  schedules_19[[x]] <- schedules_19[[x]] %>%
    mutate(Opp = ifelse(grepl("Bethel (Ind.) *",Opponent,fixed=TRUE),teams[1],ifelse(grepl("Goshen (Ind.) *",Opponent,fixed=TRUE),teams[2],ifelse(grepl("Grace (Ind.) *",Opponent,fixed=TRUE),teams[3],ifelse(grepl("Huntington (Ind.) *",Opponent,fixed=TRUE),teams[4],ifelse(grepl("Indiana Wesleyan *",Opponent,fixed=TRUE),teams[5],ifelse(grepl("Marian (Ind.) *",Opponent,fixed=TRUE),teams[6],ifelse(grepl("Mount Vernon Nazarene (Ohio) *",Opponent,fixed=TRUE),teams[7],ifelse(grepl("Spring Arbor (Mich.) *",Opponent,fixed=TRUE),teams[8],ifelse(grepl("St. Francis (Ind.) *",Opponent,fixed=TRUE),teams[9],teams[10]))))))))))
}

schedules_19 <- lapply(teams, add_elo_score)
names(schedules_19) <- teams

#creating a master schedule with all games
AllGames_19 <- do.call("rbind",schedules_19)
HomeGames_19 <- AllGames_19 %>%
  filter(str_detect(Location, "H"))
View(HomeGames_19)


#Fix extra innings issue
Opp <- HomeGames_19[["Opp_Score"]]

for (x in seq(length(Opp))){
  if (grepl("(", Opp[x], fixed = TRUE) == TRUE){
     Opp[x] <- as.integer(substr(Opp[x], 1, nchar(Opp[x])-4))
    
    }
}

HomeGames_19[["Opp_Score"]] <- Opp

#Add margin of victory column

HomeGames_19$Team_Score <- as.integer(HomeGames_19$Team_Score)
HomeGames_19$Opp_Score <- as.integer(HomeGames_19$Opp_Score)

HomeGames_19$Vict_Margin <- (HomeGames_19$Team_Score - HomeGames_19$Opp_Score)

#2020 schedules

#function that adds a binary variable
#1 if home wins/away loses and 0 if home loses/away wins
add_elo_score <- function(x){
  schedules_20[[x]] <- schedules_20[[x]] %>%
    mutate(result = ifelse(grepl("W",Decision,fixed=TRUE)&grepl("H",Location,fixed=TRUE),1,ifelse(grepl("A",Location,fixed=TRUE)&grepl("L",Decision, fixed=TRUE),1,0)))
  #change opponent name to match abbreviations
  schedules_20[[x]] <- schedules_20[[x]] %>%
    mutate(Opp = ifelse(grepl("Bethel (Ind.) *",Opponent,fixed=TRUE),teams[1],ifelse(grepl("Goshen (Ind.) *",Opponent,fixed=TRUE),teams[2],ifelse(grepl("Grace (Ind.) *",Opponent,fixed=TRUE),teams[3],ifelse(grepl("Huntington (Ind.) *",Opponent,fixed=TRUE),teams[4],ifelse(grepl("Indiana Wesleyan *",Opponent,fixed=TRUE),teams[5],ifelse(grepl("Marian (Ind.) *",Opponent,fixed=TRUE),teams[6],ifelse(grepl("Mount Vernon Nazarene (Ohio) *",Opponent,fixed=TRUE),teams[7],ifelse(grepl("Spring Arbor (Mich.) *",Opponent,fixed=TRUE),teams[8],ifelse(grepl("St. Francis (Ind.) *",Opponent,fixed=TRUE),teams[9],teams[10]))))))))))
}

schedules_20 <- lapply(teams, add_elo_score)
names(schedules_20) <- teams

#creating a master schedule with all games
AllGames_20 <- do.call("rbind",schedules_20)
HomeGames_20 <- AllGames_20 %>%
  filter(str_detect(Location, "H"))
View(HomeGames_20)


#Fix extra innings issue
Opp <- HomeGames_20[["Opp_Score"]]

for (x in seq(length(Opp))){
  if (grepl("(", Opp[x], fixed = TRUE) == TRUE){
    Opp[x] <- as.integer(substr(Opp[x], 1, nchar(Opp[x])-4))
    
  }
}

HomeGames_20[["Opp_Score"]] <- Opp

#Add margin of victory column

HomeGames_20$Team_Score <- as.integer(HomeGames_20$Team_Score)
HomeGames_20$Opp_Score <- as.integer(HomeGames_20$Opp_Score)

HomeGames_20$Vict_Margin <- (HomeGames_20$Team_Score - HomeGames_20$Opp_Score)



#Way to run elo that includes margin of victory using log

#Run elo
e <- elo.run(score(Team_Score, Opp_Score) ~ adjust(Team,24) + Opp +
               k(20*log(abs(Team_Score - Opp_Score) + 1)), data = HomeGames_19)


#print results
f <- final.elos(e)
f <- sort(f,decreasing = TRUE)
View(f)


#regress elo values to the mean
for (g in 1:length(f)){

   f[g] <- f[g] * (2/3) + 500
  
}

#Calculating Win Probability for next game

results <- elo.run(score(Team_Score, Opp_Score) ~ adjust(Team,24) + Opp +
                     k(20*log(abs(Team_Score - Opp_Score) + 1)), data = HomeGames_20, initial.elos = f, na.omit)
View(results)

#initial.elos = f[cat("\"",Team,"\"", sep=""),], f[cat("\"",Opp,"\"", sep=""),]

home_team <- "BC"
away_team <- "GRC"

nextup <- data.frame(
  Team = home_team,
  Opp = away_team
  )
home_win <- predict(results, nextup)
away_win <- (1-home_win)
ifelse(home_team == "GRC", print(home_win), print(away_win))