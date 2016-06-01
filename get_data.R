library("rvest")
library("httr")
library("stringr")
library("lubridate")
library("tidyr")
library("dplyr")
options(stringsAsFactors = F)

get_team_squad_urls <- function() {
  # Simple function to get the urls for the teams in UEFA 2016.
  # Returns a list of URLs.
  message("getting squad URLs...")
  
  # declaring the needed urls: 
  base_url <- "http://www.uefa.com"
  teams_url <- "http://www.uefa.com/uefaeuro/"
  
  # getting the html:
  teams_nodes <- read_html(teams_url)
  
  # Extracting the URLs from the links to the team pages. 
  # The URL for the team squads end in "/squad", so we have to construct that path:
  teams <-  html_nodes(teams_nodes, "a.group-item") %>%
    html_attr("href",TRUE) %>%
    str_replace("index.html","squad") %>%
    lapply(url_absolute, base_url)
  
  message("done")
  unique(teams)
}


get_player_urls <- function(squad_url) {
  # Simple function to get the urls for the teams in UEFA 2016.
  # Returns a list of URLs.
  message(str_c(c("getting player URLs from ", squad_url, " ...")))
  
  # declaring the needed urls: 
  base_url <- "http://www.uefa.com"
  
  # getting the html:
  player_nodes <- read_html(squad_url)
  
  # extracting the URLs from the links to the player pages:
  players <-  html_nodes(player_nodes, "td a") %>%
    html_attr("href",TRUE) %>%
    lapply(url_absolute, base_url)
  
  message("done")
  players
}


get_player_html <- function (p_url) {
  # A function to extract player information from UEFA player sites.
  # Expects a url string as parameter.
  # Returns a list of extracted values.
  player_data <- list()
  message(str_c(c("getting player URLs from ", p_url, " ...")))
  # Getting the HTML
  player_data$html <- read_html(p_url)
  player_data$id <- str_extract(p_url, "player=\\d+") %>%
    str_extract("\\d+")
  message("done")
  player_data
}

process_player_html <- function(player) {
  message("extracting and renaming labels...")
  # Getting and renaming the 
  player_data_labels <-  html_nodes(player$html, "ul li span.profile--list--label") %>%
    html_text(TRUE) %>%
    str_replace_all(" ", "_") %>%
    tolower()
  
  message("extracting data...")
  player_data <- html_nodes(player$html, "ul li span.profile--list--data") %>%
    html_text(TRUE) %>%
    as.list() %>%
    setNames(player_data_labels)
  player_data$id <- player$id
  player_data
}


squad_urls <- get_team_squad_urls()
player_urls <- lapply(squad_urls, get_player_urls) 
player_urls <- unlist(player_urls)
player_html <- lapply(player_urls, get_player_html)
player_data <- lapply(player_html, process_player_html)
player_data_frames <- lapply(player_data, as.data.frame)
df <- Reduce(function(x, y) merge(x, y, all=TRUE), player_data_frames)

df <- apply(df, 2, function(x){x[x == "-" | x == "()"] <- NA; x}) %>%
  as.data.frame()

df$height <- as.numeric(str_extract_all(df$height,"\\d+"))
df$weight <- as.numeric(str_extract_all(df$weight,"\\d+"))
df <- separate(df, date_of_birth_.age., c("date_of_birth", "age"), sep = " ") %>%
  separate(senior_debut, c("debut_date", "debut_game"), sep = ": ") %>%
  separate(last_appearance, c("last_appearance_date", "last_appearance_game"), sep = ":")
df$age <- extract_numeric(df$age)
dates <- select(df,contains("date")) %>% 
  lapply(dmy)


write.table(df,"data.csv", row.names = FALSE, sep = "|")
df <- read.csv("data.csv", sep = "|", encoding = "UTF-8")

summary(df)
p <- interval(df$date_of_birth, today()) %>% as.period(unit = "years")
df$age2 <- p$year
birthday_boys <- filter(df, age != age2)
df <- select(df, -age2)
