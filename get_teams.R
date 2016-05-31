# uncomment to install:
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")

library("rvest")
library("httr")
library("stringr")


get_team_squad_urls <- function() {
  # Simple function to get the urls for the teams in UEFA 2016.
  # Returns a list of URLs.
  message("getting squad URLs...")
  
  # declaring the needed urls: 
  base_url <- "http://www.uefa.com"
  teams_url <- "http://www.uefa.com/uefaeuro/"
  
  # getting the html:
  teams_nodes <- GET(teams_url) %>%
    content("text", encoding = "UTF-8") %>%
    read_html()
  
  # Extracting the URLs from the links to the team pages. 
  # The URL for the team squads end in "/squad", so we have to construct that path:
  teams <-  html_nodes(teams_nodes, "a.group-item") %>%
    html_attr("href",TRUE) %>%
    str_replace("index.html","squad") %>%
    lapply(url_absolute, base_url)
  
  message("done")
  teams
}
team_squad_urls <- get_team_squad_urls()
