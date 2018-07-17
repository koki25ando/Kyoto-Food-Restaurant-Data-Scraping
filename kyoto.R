# Analysis
# Use python to analyze dataset
# Geospatial visualization
# pie chart
# wordcloud by python
# Treemap

# Prep
library(tidyverse)
library(rvest)


main.link.list <- print(paste0("https://tabelog.com/en/kyoto/rstLst/", 1:60, "/"))

# Scrape main information

Restaurant_Info_Scraping <- function (url) {

page <- read_html(url)
data.conteiner <-
  page %>% 
  html_nodes("div#js-map-search-container") %>% 
  html_nodes("form#js-search-form") %>% 
  html_nodes("div#js-search-result") %>% 
  html_nodes("ul#js-map-search-result-list")

Restaurant.Name <- 
  data.conteiner %>% 
  html_nodes("li.list-rst") %>% 
  html_nodes("div.list-rst__header") %>% 
  html_nodes("p.list-rst__name") %>% 
  html_text()

Restaurant.link <- 
  data.conteiner %>% 
  html_nodes("li.list-rst") %>% 
  html_nodes("div.list-rst__header") %>% 
  html_nodes("p.list-rst__name") %>% 
  html_nodes("a") %>% 
  html_attr("href")

data.frame(Restaurant.Name, Restaurant.link)
}

Kyoto.Restaurant.Main <- apply(data.frame(main.link.list), 1, Restaurant_Info_Scraping)
Kyoto.Restaurant.Main.df <- do.call(rbind, Kyoto.Restaurant.Main)

# Clean dataset

Kyoto.Restaurant.Main.df$Restaurant.Name <- as.character(Kyoto.Restaurant.Main.df$Restaurant.Name)
Kyoto.Restaurant.Main.df$Restaurant.Name <- 
  Kyoto.Restaurant.Main.df$Restaurant.Name %>% 
  str_remove("\n            ")

Kyoto.Restaurant.Main.df$Restaurant.Name <- 
  Kyoto.Restaurant.Main.df$Restaurant.Name %>% 
  str_replace(pattern = "\n          ", replacement = "/")
Kyoto.Restaurant.Main.df$Restaurant.Name <- 
  Kyoto.Restaurant.Main.df$Restaurant.Name %>% 
  str_remove("\n          ")
Kyoto.Restaurant.Main.df <- 
  Kyoto.Restaurant.Main.df %>% 
  separate(Restaurant.Name, into = c("Name", "JapaneseName"), sep = "/ ")

# Export dataset
# write.csv(Kyoto.Restaurant.Main.df, file = "Kyoto_Restaurant.csv")


# Content Information
Kyoto_Restaurant_Scraping <- function (url) {
  
url <- as.character(url)

page <-
  url %>% 
  read_html()

# Name
Restaurant.name <- 
  page %>% 
  html_nodes("div.l-contents") %>% 
  html_nodes("div.rd-header__headline") %>% 
  html_nodes("h2") %>% 
  html_text()


Info.container <-
  page %>% 
  html_nodes("div.l-contents") %>% 
  html_nodes("div.rd-header__contents")

# Basic Information
Basic.info <- 
  Info.container %>% 
  html_nodes("div.rd-header__info-table") %>% 
  html_nodes("dd") %>% 
  html_text()

Station <- Basic.info[1]
Price <- Basic.info[4]

# Rating Information
Rating.info <- 
  Info.container %>% 
  html_nodes("ul.rd-header__rst-rate") %>% 
  html_text()

Location <- 
  Info.container %>% 
  html_nodes("p.rd-header__rst-map") %>% 
  html_nodes("img") %>% 
  html_attr("alt")

data.frame(Restaurant.name, Station, Price, Rating.info, Location)
}

Kyoto.Restaurant <- apply(data.frame(Kyoto.Restaurant.Main.df$Restaurant.link), 1, Kyoto_Restaurant_Scraping)
Kyoto.Restaurant.df <- do.call(rbind, Kyoto.Restaurant)

# write.csv(Kyoto.Restaurant.df, file = "Kyoto_Restaurant_Info.csv")

# Clean dataframe
Kyoto.Restaurant.df$Restaurant.name <- 
  Kyoto.Restaurant.df$Restaurant.name %>% 
  str_remove("\n          ")
Kyoto.Restaurant.df$Restaurant.name <- 
  Kyoto.Restaurant.df$Restaurant.name %>% 
  str_remove_all("\n        ")
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Restaurant.name, into = c("Name", "JapaneseName"),
           sep = "\\(")
Kyoto.Restaurant.df$JapaneseName <- 
  Kyoto.Restaurant.df$JapaneseName %>% 
  str_remove("\\)")



Kyoto.Restaurant.df$Station <- 
  Kyoto.Restaurant.df$Station %>% 
  str_remove_all("\n                  ")
Kyoto.Restaurant.df$Station <- 
  Kyoto.Restaurant.df$Station %>% 
  str_remove_all("                \n                \n              ")
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Station, sep = "\n ",
           into = c("Station", "Category"))
Kyoto.Restaurant.df$Category <- 
  Kyoto.Restaurant.df$Category %>% 
  str_replace_all(pattern = "in Kyoto", replacement = "\\,")
Kyoto.Restaurant.df$Category <- 
  Kyoto.Restaurant.df$Category %>% 
  str_remove("Kyoto")


Kyoto.Restaurant.df$Rating.info <- 
  Kyoto.Restaurant.df$Rating.info %>% 
  str_remove_all("\n              \n            \n          ")
Kyoto.Restaurant.df$Rating.info <- 
  Kyoto.Restaurant.df$Rating.info %>% 
  str_remove_all("\n            ")

Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Rating.info, sep = "Details",
           into = c("Summary_Rating", "Dinner_Lunch"))

Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Dinner_Lunch, sep = "Lunch: ",
           into = c("Dinner", "Lunch"))

Kyoto.Restaurant.df$Lunch <- 
  Kyoto.Restaurant.df$Lunch %>% 
  str_replace(pattern = "      ", replace = ",")

Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Lunch, sep = "\\,",
           into = c("Lunch", "Review"))

Kyoto.Restaurant.df$Location <- as.character(Kyoto.Restaurant.df$Location)
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_replace(pattern = "Staticmap", replacement = "")
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_remove_all("\\=")
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_remove_all("\\&")
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_remove_all("\\?")
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_remove_all("kakakucomincchanneltabelog.comsensorfalsehljacenter")
Kyoto.Restaurant.df$Location <- 
  Kyoto.Restaurant.df$Location %>% 
  str_remove_all("clientgme ")
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Location, sep = "markerscolor",
           into = c("Location", "Dummy"))
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  select(-Dummy)
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Location, sep = "\\,",
           into = c("Lat", "Long"))

Kyoto.Restaurant.df$Dinner <- 
  Kyoto.Restaurant.df$Dinner %>% 
  str_remove("  Dinner:  ")


Kyoto.Restaurant.df$Category <- 
  Kyoto.Restaurant.df$Category %>% 
  str_remove_all("                                           ")
Kyoto.Restaurant.df$Category <- 
  Kyoto.Restaurant.df$Category %>% 
  str_replace(pattern = " ,                                      ", replacement = ", ")
Kyoto.Restaurant.df$Category <- 
  Kyoto.Restaurant.df$Category %>% 
  str_replace(pattern = " ,                                      ", replacement = ", ")


Kyoto.Restaurant.df$Price <- as.character(Kyoto.Restaurant.df$Price)
Kyoto.Restaurant.df$Price <- 
  Kyoto.Restaurant.df$Price %>% 
  str_remove("\n                  ")
Kyoto.Restaurant.df$Price <- 
  Kyoto.Restaurant.df$Price %>% 
  str_remove("                ")

Kyoto.Restaurant.df$Price <- 
  Kyoto.Restaurant.df$Price %>% 
  str_remove("\\#\\{type.capitalize\\}:")
Kyoto.Restaurant.df$Price <- 
  Kyoto.Restaurant.df$Price %>% 
  str_remove("\\#\\{type.capitalize\\}")
Kyoto.Restaurant.df$Price <- 
  Kyoto.Restaurant.df$Price %>% 
  str_remove_all("\\,")

names(Kyoto.Restaurant.df)[7] <- "DinnerRating"
names(Kyoto.Restaurant.df)[8] <- "LunchRating"

Kyoto.Restaurant.df$DinnerRating <- as.numeric(Kyoto.Restaurant.df$DinnerRating)
Kyoto.Restaurant.df$LunchRating <- as.numeric(Kyoto.Restaurant.df$LunchRating)
Kyoto.Restaurant.df$Summary_Rating <- as.numeric(Kyoto.Restaurant.df$Summary_Rating)

table(is.na(Kyoto.Restaurant.df$DinnerRating))

Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Price, sep = ":",
           into = c("DinnerPrice", "LunchPrice"))

Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  filter(!is.na(DinnerRating))
names(Kyoto.Restaurant.df)[7] <- "TotalRating"
Kyoto.Restaurant.df <- 
  Kyoto.Restaurant.df %>% 
  separate(Category, sep = ", ",
           into = c("FirstCategory", "SecondCategory"))
Kyoto.Restaurant.df$Lat <- as.numeric(Kyoto.Restaurant.df$Lat)
Kyoto.Restaurant.df$Long <- as.numeric(Kyoto.Restaurant.df$Long)
Kyoto.Restaurant.df$Review <- 
  Kyoto.Restaurant.df$Review %>% 
  str_remove("    reviews")
Kyoto.Restaurant.df$Review <- as.numeric(Kyoto.Restaurant.df$Review)
names(Kyoto.Restaurant.df)[11] <- "ReviewNum"

write.csv(Kyoto.Restaurant.df, file = "Kyoto_Restaurant_Info.csv")

  




