#Auto Update Website before games

update_web <- function(team1, team2, game_loc){
  
  team1_elo <- f[team1]
  team2_elo <- f[team2]
  print(team1_elo)
  print(team2_elo)
  game_park_effect <- p_factor[game_loc]
  game_prob_team1win <- elo.prob(team1_elo, team2_elo)
  game_prob_team2win <- elo.prob(team2_elo, team1_elo)
  
  print(game_prob_team1win)
  print(game_prob_team2win)
  print(game_park_effect)
  
  
}
update_web("INWU", "GRC", "GRC")