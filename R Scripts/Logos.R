library(rvest)
library(png)
library(knitr)

logos <- c("BC","GOC","GRC","HU","INWU","MAR","MVNU","SAU","SFIN","TAYL")
names(logos) <- c(212, 705, 706, 809, 906, 1306, 1338, 1936, 1902, 2002) 

#Create the urls that we will scrape the data from
urls <- paste0("http://d21gd0ap5v1ndt.cloudfront.net/logos/i/",names(teams),"/60/100.png")

library(RCurl)
pull <- function(x){
  myurl <- paste0("http://d21gd0ap5v1ndt.cloudfront.net/logos/i/",names(teams),"/60/100.png")
  my_image <- readPNG(getURLContent(myurl))
  
  }

logos <- lapply(urls, pull)
names(logos) <- teams

sendit <- function(x) {
  file = paste0("Data/Logos", "/", x, ".png")
}
lapply(teams, sendit)

#for(i in 1:length(urls)){
#  download.file(urls[i],paste("Data/Logos/", teams[i], ".png"))
#}

Best_Players$logo <- sprintf('![](Data/Logos/%s.png)', names(logos))
View(head(Best_Players,10))
kable(head(Best_Players,10), row.names = FALSE, col.names=c("Team", "Player", "wRC+", "PA")) %>%
  kable_styling(bootstrap_options = c("striped")) %>%
  footnote(general = "*min. 20 plate appearances", general_title)


#A function to get the png file
#grab_image <- function(x) {
  #img = readPNG(x)
#}

#image <- lapply(urls, grab_image)

#Save to an Rdata file
#save(baseball, file = "Data/stats_by_type.Rdata")