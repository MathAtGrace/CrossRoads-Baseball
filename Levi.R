library(rvest)


teams <- c("BC","GOC","GRC","HU","INWU","MAR","MVNU","SAU","SFIN","TAYL")
#R lets us rename the indices of vectors.
names(teams) <- c(1629, 1678, 1679, 1688, 1694, 1717, 1736, 1780, 1805, 1784)
  p_effects <- read_html("http://www.dakstats.com/WebSync/Pages/Team/TeamSchedule.aspx?association=10&sg=MBA&sea=NAIMBA_2019&team=1629")
  
  tbls_effects <- p_effects %>%
    html_nodes("table") %>%
    .[36] %>%
    html_table(fill = TRUE)
  
  Team_Schedule <- data.frame(tbls_effects[[1]])
  Team_Schedule <- Team_Schedule[!apply(is.na(Team_Schedule) | Team_Schedule == "", 1, all),]
  
  #Create new dataframe using only conference games. (There has to be a better way to do this.)
  
  Sched <- Team_Schedule["X2"]
  Scores <- c()
  Conf_Games <- data.frame(Date=character(),
                           Opponent=character(), 
                           Location=character(),
                           Score=character(),
                           Result=character(),
                           stringsAsFactors=FALSE) 
  j=1
  for (i in 1:nrow(Sched)){
  
    if (grepl("*", Sched[[1]][[i]], fixed = TRUE) == TRUE){
      Conf_Games[j,] <- Team_Schedule[i,]
      j = j + 1
    } 
  }
  
  #print(Conf_Games)
  
  #Separate Home games from neutral and away games
  Home_Games <- data.frame(Date=character(),
                           Opponent=character(), 
                           Location=character(),
                           Score=character(),
                           Result=character(),
                           stringsAsFactors=FALSE)
  
  Away_Games <- data.frame(Date=character(),
                           Opponent=character(), 
                           Location=character(),
                           Score=character(),
                           Result=character(),
                           stringsAsFactors=FALSE)
  
  
  Loc <- Conf_Games["Location"]

  w=0
  z=0
  for (k in 1:nrow(Loc)){
    if (is.element("H", Loc[[1]][[k]]) == TRUE){
      Home_Games[w,] <- Conf_Games[k,]
      w = w + 1
    }
    else if (is.element("A", Loc[[1]][[k]]) == TRUE){
      Away_Games[z,] <- Conf_Games[k,]
      z = z + 1
    }
   
  }
  
  #Put Home team score for each game into a vector
    

    Scores_H <- Home_Games[["Score"]]
    Scores_A <- Away_Games[["Score"]]
    p=1
    q=1
    c=1
    d=1
    
      Team_Runs_A <- c()
      Team_Runs_H <- c()
      Opp_Runs_H <- c()
      Opp_Runs_A <- c()
      
      for (x in Scores_H){
        
        Team_Runs_H[p] <- as.integer(substr(x, 1, (regexpr(pattern ='-', x)-1)))
        Opp_Runs_H[d] <- as.integer(substr(x, (regexpr(pattern ='-', x)+1), nchar(x)))
        p = p + 1
        d = d + 1

      }
      
      for (r in Scores_A){
        
        Team_Runs_A[q] <- as.integer(substr(r, 1, (regexpr(pattern ='-', r)-1)))
        Opp_Runs_A[c] <- as.integer(substr(r, (regexpr(pattern ='-', r)+1), nchar(r)))
        q = q + 1
        c = c + 1
        
      }
     

Park_effect <- ((sum(Team_Runs_H)+sum(Opp_Runs_H))/length(Team_Runs_H))/((sum(Team_Runs_A)+sum(Opp_Runs_A))/length(Team_Runs_A))

print(Park_effect)

