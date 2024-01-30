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

#register google API Key

apiKey <- register_google("Insert Google API Key here")

readRenviron("~/.Renviron")

options(tigris_use_cache = TRUE)


#Charlotte Demographics. 
NC_counties <- read.csv("NC Counties.csv")
typeof(NC_counties)



NC_race <- get_decennial(
  geography = "tract",
  state = "NC",
  county = "Mecklenburg",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>% mutate(percent = 100 * (ifelse(summary_value == 0, 0, value / summary_value)))



NC_black <- filter(NC_race,
                        variable == "Black")
NC_hispanic <- filter(NC_race, 
                           variable == "Hispanic")
NC_white <- filter(NC_race, 
                        variable == "White")
NC_asian <- filter(NC_race, 
                        variable == "Asian")

#Plotting the population distribution by ethnicity 
tm_shape(NC_black) + 
  tm_polygons(col="percent", style = "quantile", n=8, palette ="Purples", title="Estimates") +
  tm_layout(title = "Percent Black \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)

tm_shape(NC_hispanic) + 
  tm_polygons(col="percent", style = "quantile", n=8, palette ="Reds", title="Estimates") +
  tm_layout(title = "Percent Hispanic \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)

tm_shape(NC_asian) + 
  tm_polygons(col="percent", style = "quantile", n=8, palette ="Greens", title="Estimates") +
  tm_layout(title = "Percent Asian \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)

options(tigris_use_cache = TRUE)

#Using Census data to get the income distribution for Charlotte

Charlotte_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "NC", 
  county = c("Mecklenburg"),
  year = 2020,
  geometry = TRUE
)

#Plotting the distribution of income

ggplot(data = Charlotte_income, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) + 
  labs(title = "Charlotte's Estimated Income",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "ACS estimate")

mapview(Charlotte_income, zcol = "estimate")


#getting the city map of Charlotte from google API

Charlotte_map <- get_map(location = "Charlotte",maptype = "roadmap", color = "color")


#finding the locations of restaurants in Charlotte for specialty food chains

Jamba <- google_places(search_string = "Jamba Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
Cinnabon <- google_places(search_string = "cinnabon Charlotte, North Carolina",radius=50000, key = "Insert Google API Key here")
Auntie <- google_places(search_string = "Auntie Anne's Charlotte, North Carolina",radius=50000, key = "Insert Google API Key here")
Carvel <-google_places(search_string = "CarvelCharlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")

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

#combining all specialty stores to map them withing city limits

FocusBrands_Charlotte <- rbind(Jamba_geom, Cinnabon_geom, Auntie_geom, Carvel_geom)

#plotting the spcialty stores

ggmap(Charlotte_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=FocusBrands_Charlotte, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Focus Brands Specialty Stores",
       subtitle = "City of Charlotte",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Fining major competitors for ice creams

BaskinRobbins <- google_places(search_string = "Baskin Robbins Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
DairyQueen <- google_places(search_string = "Dairy Queen Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
ColdStone <- google_places(search_string = "Cold Stone Creamery Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
MarbleSlab <-google_places(search_string = "Marble Slab Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
HaagenDaz <- google_places(search_string = "Haagen Daz Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")

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


Ice_Cream_Charlotte <- rbind(BaskinRobbins_geom, DairyQueen_geom, ColdStone_geom, MarbleSlab_geom, HaagenDaz_geom)



Charlotte_map <- get_map(location = "Charlotte",maptype = "roadmap", color = "color")

#Mapping the competition for ice cream stores

ggmap(Charlotte_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Ice_Cream_Charlotte, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Ice Cream Competition",
       subtitle = "City of Charlotte",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#Gathering locations for baked goods stores

Cinnaholic <- google_places(search_string = "Cinnaholic Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
GreatAmericanCookies <-google_places(search_string = "Great American Cookies Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
CrumblCookies <- google_places(search_string = "Crumbl Cookies Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
Wetzels <- google_places(search_string = "Wetzel's Pretzels Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
PretzelTime <- google_places(search_string = "Pretzel Time Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
Bakeries <- google_places(search_string = "Bakeries Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
KrispyKreme <- google_places(search_string = "Krispy Kreme Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")

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

Snack_Charlotte <- rbind(Cinnaholic_geom, GreatAmericanCookies_geom, CrumblCookies_geom, Wetzels_geom, PretzelTime_geom, Bakeries_geom, KrispyKreme_geom)

#mapping the competition in baked goods

ggmap(Charlotte_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Snack_Charlotte, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Snacks Competition",
       subtitle = "City of Charlotte",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))

#extracting locations for smoothies and juice stores

TropicalSmoothie <- google_places(search_string = "Tropical Smoothie Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
PlanetSmoothie <- google_places(search_string = "Planet Smoothie Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")
SmoothieKing <- google_places(search_string = "Smoothie King Charlotte, North Carolina",radius=50000,  key = "Insert Google API Key here")

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

Smoothies_Charlotte <- rbind(TropicalSmoothie_geom, PlanetSmoothie_geom, SmoothieKing_geom)

#Mapping juices and smoothies competitors

ggmap(Charlotte_map, extent = "panel", maprange = TRUE, base_layer =  ggplot(data=Smoothies_Charlotte, aes(lng, lat, colour = name))) +
  geom_point(size =5)+
  scale_fill_hue(c= 300,l=100)+
  labs(title = "Smoothies Competition",
       subtitle = "City of Charlotte",
       x = "Longitude",
       y = "Latitude",
       color = "Stores")+
  theme(plot.title = element_text(size = 20), plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15), legend.title = element_text(size =15))
