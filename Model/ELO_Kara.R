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

#Add margin of victory column

HomeGames$Team_Score <- as.integer(HomeGames$Team_Score)
HomeGames$Opp_Score <- as.integer(HomeGames$Opp_Score)

HomeGames$Vict_Margin <- (HomeGames$Team_Score - HomeGames$Opp_Score)



#Way to run elo that includes margin of victory using log

#Run elo
e <- elo.run(score(HomeGames[["Team_Score"]], HomeGames[["Opp_Score"]]) ~ HomeGames$Team + HomeGames$Opp +
               k(20*log(abs(HomeGames[["Team_Score"]] - HomeGames[["Opp_Score"]]) + 1)), data = HomeGames)


#print results
f <- final.elos(e)
f <- sort(f,decreasing = TRUE)




#regress elo values to the mean
for (g in 1:length(f)){

   f[g] <- f[g] * (2/3) + 500
  
}

View(f)

