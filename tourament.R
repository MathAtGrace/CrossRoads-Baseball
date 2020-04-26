


#custom probability function to add randomness for tournament
custom_prob <- function(elo.A, elo.B, adjust.A, adjust.B)
{
  1/(1 + 10^(((elo.B + adjust.B) - (elo.A + ifelse(elo.A > 1500, adjust.A / 2, adjust.A)))/400.0))
}


# <- elo.run2(score(points.Home, points.Visitor) ~ adjust(team.Home, 10) + team.Visitor,
      #          data = tournament, k = 20, prob.fun = custom_prob, update.fun = custom_update)

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
    View(tourney_matchups)
  }
  
}






#need a loop with win probabilities for each game and adjustment of elo values
for (games in 1:nrow(tourney_matchups)){

  Home_tourney_temp <- tourney_matchups[games,1]
  Away_tourney_temp <- tourney_matchups[games,2]
 
  tourney_probs <- elo.prob(f[toString(Home_tourney_temp)], f[toString(Away_tourney_temp)])
  #probability that home beats away
  print(tourney_probs)

}


#In progress
elo_tourney <- elo.run2(score(Team_Score, Opp_Score) ~ adjust(Team,24) + Opp +
               k(20*log(abs(Team_Score - Opp_Score) + 1)), prob.fun = custom_prob, data = HomeGames_19)

print(final.elos(elo_tourney))


