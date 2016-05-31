# uncomment to install:
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")

library("rvest")
library("httr")
library("stringr")
library("dplyr")

get_player_urls <- function(squad_url) {
  # Simple function to get the urls for the teams in UEFA 2016.
  # Returns a list of URLs.
  message(str_c(c("getting player URLs from ", squad_url, " ...")))
  
  # declaring the needed urls: 
  base_url <- "http://www.uefa.com"
  
  # getting the html:
  player_nodes <- GET(squad_url) %>%
    content("text") %>%
    read_html()
  
  # extracting the URLs from the links to the player pages:
  players <-  html_nodes(player_nodes, "td a") %>%
    html_attr("href",TRUE) %>%
    lapply(url_absolute, base_url)
  
  message("done")
  players
}
player_urls <- get_player_urls("http://www.uefa.com/uefaeuro/season=2016/teams/team=47/squad")
