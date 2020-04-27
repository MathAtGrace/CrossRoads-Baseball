#coaches poll
ranks <- c(1:10)
team_ranks <- c("HU", "MVNU", "INWU", "TAYL", "MAR", "SAU", "BC", "GOC", "GRC", "SFIN")
adjustment <- c()

#adjustment for coaches poll (function can be changed)
for (counter in ranks){
  
  adjust_value <- counter*-2
  adjustment[counter] <- adjust_value
  #print(adjustment)
}



coaches_poll <- data.frame(ranks, adjustment, row.names = team_ranks)


#custom probability function to add randomness for tournament
#custom_prob <- function(elo.A, elo.B, adjust.A, adjust.B)
#{
 # 1/(1 + 10^(((elo.B + adjust.B) - (elo.A + ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))/400.0))
#}


#Create dataframe of all matchups (add loop to have teams play multiple times)
Home_tourney_team <- c()
Away_tourney_team <- c()
tourney_matchups <- data.frame(Home_tourney_team, Away_tourney_team)


for (i in 1:(length(teams)-1)){
  for (j in 1:(length(teams)-i)){
    matchup <- data.frame(
      Home_tourney_team = teams[i],
      Away_tourney_team = teams[i+j]
    )
    #names(matchup) <- c("Home", "Away")
    tourney_matchups <- rbind(tourney_matchups, matchup)
    
  }
  
}

tourney_matchups_randomized <- tourney_matchups[sample(nrow(tourney_matchups)),]


tourney_probs <- c()
winners <- c()
Home_elos <- c()
Away_elos <- c()
#Use After_2020_games_elos for elos after 2020 games or f for elos after 2019
tourney_elos <- After_2020_games_elos
#need a loop with win probabilities for each game and adjustment of elo values
for (games in 1:nrow(tourney_matchups_randomized)){
  
  
  #get teams for game
  Home_tourney_temp <- tourney_matchups_randomized[games,1]
  Away_tourney_temp <- tourney_matchups_randomized[games,2]
  #get and adjust elo values
  Home_tourney_temp_elo <- tourney_elos[toString(Home_tourney_temp)] + rnorm(1, mean=0, sd=25)#+ coaches_poll[toString(Home_tourney_temp), "adjustment"]
  Away_tourney_temp_elo <- tourney_elos[toString(Away_tourney_temp)] + rnorm(1, mean=0, sd=25)#+ coaches_poll[toString(Away_tourney_temp), "adjustment"]
  
  #add elos to vector for dataframe
  Home_elos[games] <- Home_tourney_temp_elo
  Away_elos[games] <- Away_tourney_temp_elo
  
  #get home team win probability
  game_probs <- elo.prob(Home_tourney_temp_elo, Away_tourney_temp_elo)#elo.prob(f[toString(Home_tourney_temp)], f[toString(Away_tourney_temp)])
  
  #keep track of winners
  if (game_probs > 0.5){
    
    winners[games] <- toString(Home_tourney_temp)
    win_check <- 1
    
  }
  else{
    
    winners[games] <- toString(Away_tourney_temp)
    win_check <- 0
    
  }
  
  
  
  
  Home_team_pred_score <- round(10*game_probs)
  Away_team_pred_score <- round(10*(1-game_probs))
  
  #caculate new elo values based on predicted score
  run_elo_sim <- elo.calc(win_check, Home_tourney_temp_elo, Away_tourney_temp_elo, k(20*log(abs(Home_team_pred_score - Away_team_pred_score) + 1)))
  
  #change elo values in table f_copy
  tourney_elos[toString(Home_tourney_temp)] <- as.double(run_elo_sim["elo.A"])
  tourney_elos[toString(Away_tourney_temp)] <- as.double(run_elo_sim["elo.B"])
  
  
  #probability that home beats away
  #print(tourney_probs)
  tourney_probs <- c(tourney_probs, game_probs)

  

}

#tourney_matchups <- cbind(tourney_matchups, winners)

tourney_matchups_randomized <- cbind(tourney_matchups_randomized, Home_elos, Away_elos, tourney_probs, winners)

tourney_elos <- sort(tourney_elos,decreasing = TRUE)

#tail(tourney_elos, n=1)

