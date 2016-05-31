library("ggplot2")
library("plotly")
library("leaflet")
library("SPARQL")
library("dplyr")
library("tidyr")
library("stringr")
library("httr")
options(stringsAsFactors = F)
endpoint <- "https://query.wikidata.org//sparql"
query <- "
select ?stadium ?stadiumLabel ?location ?capacity ?image ?commune ?communeLabel ?communeImage where {
	wd:Q189571 wdt:P276 ?stadium .
?stadium wdt:P625 ?location .
?stadium wdt:P1083 ?capacity .
?stadium wdt:P18 ?image .
?stadium wdt:P131 ?commune .
?commune wdt:P18 ?communeImage .
SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }

}"

stadiums <- SPARQL(endpoint, query = query)$result

write.csv(stadiums, "stadiums.csv")
stadiums <- read.csv("stadiums.csv", encoding = "UTF-8")

stadiums <- stadiums %>% 
  separate(location, c("long", "lat"), sep = " ")
stadiums$long = extract_numeric(stadiums$long)
stadiums$lat = as.numeric(str_replace_all(stadiums$lat, "\\).*", ""))

ggplot(stadiums, mapping = (aes(stadiumLabel, capacity, color = stadiumLabel, fill = stadiumLabel))) +
  geom_bar(stat = "identity")
ggplotly(tooltip = c("x","y"))


viz <- ggplot(stadiums, aes(long, lat)) +
  borders(regions = "france") +
  geom_point(aes(text = stadiumLabel, size = capacity), colour = "#2299ff", alpha = 0.7)
ggplotly(viz)

map1 <- leaflet() %>%
  addProviderTiles()
  addTiles() %>% 
  setView(lng = 2.360104, lat = 48.92444, zoom = 15) %>% 
  addCircleMarkers(lng = 2.360104, lat = 48.92444, popup = "Stade de France", opacity = 0.7, weight = 0.5, radius = 200)
map1

