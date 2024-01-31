install.packages('tidycensus')
install.packages('tmap')
install.packages('ggmap')
install.packages('rstudioapi')
install.packages('mapview')
install.packages("googleway")

library(ggmap)
library(rstudioapi)
library(tidycensus)
library(tidyverse)
library(tigris)
library(mapview)
library(tmap)
library(googleway)

setwd('~/PACE/R scripts')


apiKey <- register_google("Insert google API Key here")

options(tigris_use_cache = TRUE)

#Getting income distribution for Raleigh, NC

Raleigh_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "NC", 
  county = c("Wake", "Durham"),
  year = 2020,
  geometry = TRUE
)

#Plotting the income distribution for Raleigh

mapview(Raleigh_income, zcol = "estimate")

#Fetching the google map for the city of Raleigh

Raleigh_map <- get_map(location = "Raleigh",maptype = "roadmap", color = "color")

#finding restaurant locations in the city of Raleigh

Jamba <- google_places(search_string = "Jamba Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
Cinnabon <- google_places(search_string = "cinnabon Raleigh, North Carolina",radius=50000, key = "Insert google API Key here")
Auntie <- google_places(search_string = "Auntie Anne's Raleigh, North Carolina",radius=50000, key = "Insert google API Key here")
Carvel <-google_places(search_string = "CarvelRaleigh, North Carolina",radius=50000,  key = "Insert google API Key here")

Jamba_Locations <- c(Jamba$results)
Jamba_geom <- c(Jamba_Locations$geometry)
Jamba_geom <-Jamba_geom$location
Jamba_geom$name <-c("Jamba Juice")

Cinnabon_Locations <- Cinnabon$results
Cinnabon_geom <- Cinnabon_Locations$geometry
Cinnabon_geom <- Cinnabon_geom$location
Cinnabon_geom$name <- c("Cinnabon")

Auntie_Locations <- Auntie$results
Auntie_geom <- Auntie_Locations$geometry
Auntie_geom <- Auntie_geom$location
Auntie_geom$name <- c("Auntie Anne's")

Carvel_Locations <- c(Carvel$results)
Carvel_geom <- Carvel_Locations$geometry
Carvel_geom <- Carvel_geom$location
Carvel_geom$name <- c("Carvel")

FocusBrands_Raleigh <- rbind(Jamba_geom, Cinnabon_geom, Auntie_geom, Carvel_geom)

#Plotting the restaurant locations within the city of Raleigh

ggmap(Raleigh_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=FocusBrands_Raleigh, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Focus Brands Specialty Stores",
       subtitle = "City of Raleigh",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Fiding the location of ice cream competitors

BaskinRobbins <- google_places(search_string = "Baskin Robbins Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
DairyQueen <- google_places(search_string = "Dairy Queen Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
ColdStone <- google_places(search_string = "Cold Stone Creamery Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
MarbleSlab <-google_places(search_string = "Marble Slab Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
HaagenDaz <- google_places(search_string = "Haagen Daz Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")

BaskinRobbins_Locations <- c(BaskinRobbins$results)
BaskinRobbins_geom <- c(BaskinRobbins_Locations$geometry)
BaskinRobbins_geom <-BaskinRobbins_geom$location
BaskinRobbins_geom$name <- c("Baskin Robins")

DairyQueen_Locations <- c(DairyQueen$results)
DairyQueen_geom <- c(DairyQueen_Locations$geometry)
DairyQueen_geom <-DairyQueen_geom$location
DairyQueen_geom$name <- c("Dairy Queen")

ColdStone_Locations <- c(ColdStone$results)
ColdStone_geom <- c(ColdStone_Locations$geometry)
ColdStone_geom <- ColdStone_geom$location
ColdStone_geom$name <- c("Cold Stone")

MarbleSlab_Locations <- c(MarbleSlab$results)
MarbleSlab_geom <- c(MarbleSlab_Locations$geometry)
MarbleSlab_geom <- MarbleSlab_geom$location
MarbleSlab_geom$name <- c("Marble Slab")

HaagenDaz_Locations <- c(HaagenDaz$results)
HaagenDaz_geom <- c(HaagenDaz_Locations$geometry)
HaagenDaz_geom <- HaagenDaz_geom$location
HaagenDaz_geom$name <- c("Haagen Daz")


Ice_Cream_Raleigh <- rbind(BaskinRobbins_geom, DairyQueen_geom, ColdStone_geom, MarbleSlab_geom, HaagenDaz_geom)


Raleigh_map <- get_map(location = "Raleigh",maptype = "roadmap", color = "color")

#Plotting the location of the ice cream competitors in Raleigh

ggmap(Raleigh_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Ice_Cream_Raleigh, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Ice Cream Competition",
       subtitle = "City of Raleigh",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Fiding the location of snacks and baked good competitors

Cinnaholic <- google_places(search_string = "Cinnaholic Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
GreatAmericanCookies <-google_places(search_string = "Great American Cookies Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
CrumblCookies <- google_places(search_string = "Crumbl Cookies Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
Wetzels <- google_places(search_string = "Wetzel's Pretzels Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
PretzelTime <- google_places(search_string = "Pretzel Time Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
Bakeries <- google_places(search_string = "Bakeries Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
KrispyKreme <- google_places(search_string = "Krispy Kreme Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")

Cinnaholic_Locations <- c(Cinnaholic$results)
Cinnaholic_geom <- c(Cinnaholic_Locations$geometry)
Cinnaholic_geom <-Cinnaholic_geom$location
Cinnaholic_geom$name <- c("Cinnaholic")

GreatAmericanCookies_Locations <- c(GreatAmericanCookies$results)
GreatAmericanCookies_geom <- c(GreatAmericanCookies_Locations$geometry)
GreatAmericanCookies_geom <-GreatAmericanCookies_geom$location
GreatAmericanCookies_geom$name <- c("Great American Cookies")

CrumblCookies_Locations <- c(CrumblCookies$results)
CrumblCookies_geom <- c(CrumblCookies_Locations$geometry)
CrumblCookies_geom <-CrumblCookies_geom$location
CrumblCookies_geom$name <- c("Crumbl Cookies")

Wetzels_Locations <- c(Wetzels$results)
Wetzels_geom <- c(Wetzels_Locations$geometry)
Wetzels_geom <-Wetzels_geom$location
Wetzels_geom$name <- c("Wetzels")

PretzelTime_Locations <- c(PretzelTime$results)
PretzelTime_geom <- c(PretzelTime_Locations$geometry)
PretzelTime_geom <-PretzelTime_geom$location
PretzelTime_geom$name <- c("Pretzel Time")

Bakeries_Locations <- c(Bakeries$results)
Bakeries_geom <- c(Bakeries_Locations$geometry)
Bakeries_geom <-Bakeries_geom$location
Bakeries_geom$name <- c("Bakeries")

KrispyKreme_Locations <- c(KrispyKreme$results)
KrispyKreme_geom <- c(KrispyKreme_Locations$geometry)
KrispyKreme_geom <-KrispyKreme_geom$location
KrispyKreme_geom$name <- c("Krispy Kreme")

Snack_Raleigh <- rbind(Cinnaholic_geom, GreatAmericanCookies_geom, CrumblCookies_geom, Wetzels_geom, PretzelTime_geom, Bakeries_geom, KrispyKreme_geom)

#Plotting the snacks and baked good competitors

ggmap(Raleigh_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Snack_Raleigh, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Snacks Competition",
       subtitle = "City of Raleigh",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Finding the location of juice and smoothies competitors

TropicalSmoothie <- google_places(search_string = "Tropical Smoothie Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
PlanetSmoothie <- google_places(search_string = "Planet Smoothie Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")
SmoothieKing <- google_places(search_string = "Smoothie King Raleigh, North Carolina",radius=50000,  key = "Insert google API Key here")

TropicalSmoothie_Locations <- c(TropicalSmoothie$results)
TropicalSmoothie_geom <- c(TropicalSmoothie_Locations$geometry)
TropicalSmoothie_geom <-TropicalSmoothie_geom$location
TropicalSmoothie_geom$name <- c("Tropical Smoothie")

PlanetSmoothie_Locations <- c(PlanetSmoothie$results)
PlanetSmoothie_geom <- c(PlanetSmoothie_Locations$geometry)
PlanetSmoothie_geom <-PlanetSmoothie_geom$location
PlanetSmoothie_geom$name <- c("Planet Smoothie")

SmoothieKing_Locations <- c(SmoothieKing$results)
SmoothieKing_geom <- c(SmoothieKing_Locations$geometry)
SmoothieKing_geom <-SmoothieKing_geom$location
SmoothieKing_geom$name <- c("Smoothie King")

Smoothies_Raleigh <- rbind(TropicalSmoothie_geom, PlanetSmoothie_geom, SmoothieKing_geom)

#Plotting the juice and smoothies competition

ggmap(Raleigh_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Smoothies_Raleigh, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Smoothies Competition",
       subtitle = "City of Raleigh",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))
