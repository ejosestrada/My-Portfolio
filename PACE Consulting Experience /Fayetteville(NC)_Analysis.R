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

#registering google API key
apiKey <- register_google("Insert Google API Key Here")

options(tigris_use_cache = TRUE)

#Extracting data from Census API to get income distribution

Fayetteville_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "NC", 
  county = c("Cumberland"),
  year = 2020,
  geometry = TRUE
)

#Plotting incvome distribution for Fayetteville

mapview(Fayetteville_income, zcol = "estimate")


#Fetching the Google map for Fayeteville

Fayetteville_map <- get_map(location = "Fayetteville, NC, USA",maptype = "roadmap", color = "color")

#Locating the restaurants locations within the city 
Jamba <- google_places(search_string = "Jamba Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
Cinnabon <- google_places(search_string = "cinnabon Fayetteville, North Carolina",radius=50000, key = "Insert Google API Key Here")
Auntie <- google_places(search_string = "Auntie Anne's Fayetteville, North Carolina",radius=50000, key = "Insert Google API Key Here")
Carvel <-google_places(search_string = "CarvelFayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")


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

FocusBrands_Fayetteville <- rbind(Jamba_geom, Cinnabon_geom, Auntie_geom)

#Plotting the restaurant locations in Fayeteville

ggmap(Fayetteville_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=FocusBrands_Fayetteville, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Focus Brands Specialty Stores",
       subtitle = "City of Fayetteville",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Finding Locations for ice cream competitors

BaskinRobbins <- google_places(search_string = "Baskin Robbins Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
DairyQueen <- google_places(search_string = "Dairy Queen Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
ColdStone <- google_places(search_string = "Cold Stone Creamery Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
MarbleSlab <-google_places(search_string = "Marble Slab Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
HaagenDaz <- google_places(search_string = "Haagen Daz Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")

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


Ice_Cream_Fayetteville <- rbind(BaskinRobbins_geom, DairyQueen_geom, ColdStone_geom, MarbleSlab_geom, HaagenDaz_geom)



Fayetteville_map <- get_map(location = "Fayetteville, NC, USA",maptype = "roadmap", color = "color")

#Mapping the ice cream competition

ggmap(Fayetteville_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Ice_Cream_Fayetteville, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Ice Cream Competition",
       subtitle = "City of Fayetteville",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Finding the competitor locations for baked goods

Cinnaholic <- google_places(search_string = "Cinnaholic Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
GreatAmericanCookies <-google_places(search_string = "Great American Cookies Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
CrumblCookies <- google_places(search_string = "Crumbl Cookies Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
Wetzels <- google_places(search_string = "Wetzel's Pretzels Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
PretzelTime <- google_places(search_string = "Pretzel Time Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
Bakeries <- google_places(search_string = "Bakeries Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
KrispyKreme <- google_places(search_string = "Krispy Kreme Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")

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

Snack_Fayetteville <- rbind(Cinnaholic_geom, GreatAmericanCookies_geom, CrumblCookies_geom, Wetzels_geom, PretzelTime_geom, Bakeries_geom, KrispyKreme_geom)

#Plotting the location of competitors in snacks and baked goods

ggmap(Fayetteville_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Snack_Fayetteville, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Snacks Competition",
       subtitle = "City of Fayetteville",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Locating the competition for juices and smoothies

TropicalSmoothie <- google_places(search_string = "Tropical Smoothie Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
PlanetSmoothie <- google_places(search_string = "Planet Smoothie Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")
SmoothieKing <- google_places(search_string = "Smoothie King Fayetteville, North Carolina",radius=50000,  key = "Insert Google API Key Here")

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

Smoothies_Fayetteville <- rbind(TropicalSmoothie_geom, PlanetSmoothie_geom, SmoothieKing_geom)

#Plotting the competitors location for smoothies

ggmap(Fayetteville_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Smoothies_Fayetteville, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Smoothies Competition",
       subtitle = "City of Fayetteville",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))
