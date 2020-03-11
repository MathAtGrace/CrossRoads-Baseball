#Auto Update Website before games

update_web <- function(team1, team2, game_loc){
  
  team1_elo <- f[team1]
  team2_elo <- f[team2]
  print(team1_elo)
  print(team2_elo)
  game_park_effect <- p_factor[game_loc]
  print(game_park_effect)
  
  
}
update_web("INWU", "GRC", "GRC")