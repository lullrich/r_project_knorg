# uncomment to install:
# install.packages("rvest")
# install.packages("httr")
# install.packages("stringr")

library("rvest")
library("httr")
library("stringr")

get_player_data <- function (url) {
  # A function to extract player information from UEFA player sites.
  # Expects a url string as parameter.
  # Returns a list of extracted values.
  
  message("getting HTTP response...")
  # Getting the HTML
  player_data_nodes <- GET(url) %>%
    content("text", encoding = "UTF-8") %>%
    read_html()
  
  message("extracting and renaming labels...")
  # Getting and renaming the labels
  player_data_labels <-  html_nodes(player_data_nodes, "ul li span.profile--list--label") %>%
    html_text(TRUE) %>%
    str_replace_all(" ", "_") %>%
    tolower()
  
  message("extracting data...")
  player_data <- html_nodes(player_data_nodes, "ul li span.profile--list--data") %>%
    html_text(TRUE) %>%
    as.list() %>%
    setNames(player_data_labels)
  
  # extracting and storing the id (from the url)
  player_data$id <- str_extract(url_, "player=\\d+") %>%
    str_extract("\\d+")
  message("done")
  player_data
}
url_ <- "http://www.uefa.com/uefaeuro/season=2016/teams/player=250008930/index.html"
df <- get_player_data(url_)
