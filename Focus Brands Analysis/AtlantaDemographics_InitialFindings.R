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

#registering Google API

apiKey <- register_google("Insert Google API Here")

#getting the Map of Atlanta
Atlanta_Map <- ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color"))
print(Atlanta_Map)


readRenviron("~/.Renviron")

options(tigris_use_cache = TRUE)

#getting the tracts from Census data for metro Atlanta to map income distribution

Atlanta_income <- get_acs(
  geography = "tract", 
  variables = "B19013_001",
  state = "GA", 
  county = c("Fulton", "Gwinnet", "Cherokee","Clayton","Cobb","DeKalb","Douglas","Fayette","Forsyth","Henry","Rockdale"),
  year = 2020,
  geometry = TRUE
)

#Plotting the income distribution

ggplot(data = Atlanta_income, aes(fill = estimate)) + 
  geom_sf() + 
  scale_fill_distiller(palette = "RdPu", 
                       direction = 1) + 
  labs(title = "Atlanta's Estimated Income",
       caption = "Data source: 2020 1-year ACS, US Census Bureau",
       fill = "ACS estimate")


tm_shape(Atlanta_income) + 
  tm_polygons(col = "estimate")

#extracting ethnicity from Census API for Metro Atlanta

Atlanta_race <- get_decennial(
  geography = "tract",
  state = "GA",
  county = c("Fulton", "Gwinnet", "Cherokee","Clayton","Cobb","DeKalb","Douglas","Fayette","Forsyth","Henry","Rockdale"),
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))


#renaming variables 

Atlanta_black <- filter(Atlanta_race,
                        variable == "Black")
Atlanta_hispanic <- filter(Atlanta_race, 
                        variable == "Hispanic")
Atlanta_white <- filter(Atlanta_race, 
                        variable == "White")
Atlanta_asian <- filter(Atlanta_race, 
                        variable == "Asian")
?tm_polygons

#mapping and plotting ethnic distributions

tm_shape(Atlanta_black) + 
  tm_polygons(col="percent", style = "quantile", n=7, palette ="Purples", title="Estimates") +
  tm_layout(title = "Percent Black \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)


tm_shape(Atlanta_hispanic) + 
  tm_polygons(col="percent", style = "quantile", n=7, palette ="Reds", title="Estimates") +
  tm_layout(title = "Percent Hispanic \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)



tm_shape(Atlanta_asian) + 
  tm_polygons(col="percent", style = "quantile", n=7, palette ="Greens", title="Estimates") +
  tm_layout(title = "Percent Asian \nby Census Tract 2020", frame = FALSE, legend.outside =TRUE)


mapview(Atlanta_income, zcol = "estimate") + mapview(Atlanta_black, zcol = "percent") + mapview(Atlanta_asian, zcol = "percent")+
  mapview(Atlanta_hispanic, zcol = "percent")

#plotting ethnicity as dot densities

Atlanta_dots <- Atlanta_race %>%
  as_dot_density(
    value = "value",
    values_per_dot = 200,
    group = "variable"
  )

ggplot() +
  geom_sf(data = Atlanta_race,
          fill = "white",
          color = "grey") +
  geom_sf(data = Atlanta_dots,
          aes(color = variable),
          size = 0.01) +
  theme_void()

background_tracts <- filter(Atlanta_race, variable == "White")

tm_shape(Atlanta_race) + 
  tm_polygons(col = "white", 
              border.col = "grey") + 
  tm_shape(Atlanta_dots) +
  tm_dots(col = "variable", 
          palette = "Set1",
          size = 0.05, 
          title = "1 dot = 200 people") + 
  tm_layout(legend.outside = TRUE,
            title = "Race/ethnicity,\n2020 US Census")

tm_shape(Atlanta_race) + 
  tm_facets(by = "variable", scale.factor = 4) + 
  tm_fill(col = "percent",
          style = "quantile",
          n = 6,
          palette = "Blues",
          title = "Percent (2020 US Census)",) + 
  tm_layout(bg.color = "grey", 
            legend.position = c(-0.7, 0.15),
            panel.label.bg.color = "white")

########################################################
########################################################

#Extracting locations of restaurants in Atlanta

Jamba <- google_places(search_string = "Jamba Atlanta, Georgia",radius=50000,  key = "Insert Google API Here")
Moes <- google_places(search_string = "Moes Atlanta, Georgia",radius=50000, key = "Insert Google API Here")
Cinnabon <- google_places(search_string = "cinnabon Atlanta, Georgia",radius=50000, key = "Insert Google API Here")
Auntie <- google_places(search_string = "Auntie Anne's Atlanta, Georgia",radius=50000, key = "Insert Google API Here")
Mcalisters <- google_places(search_string = "McAlister Atlanta, Georgia",radius=50000, key = "Insert Google API Here")
Schlotzkys <- google_places(search_string = "Schlotzkys Atlanta, Georgia",radius=50000, key = "Insert Google API Here")

#arranging the geometric locations
Jamba_Locations <- c(Jamba$results)
Jamba_geom <- c(Jamba_Locations$geometry)
Jamba_geom <-Jamba_geom$location

Moes_Locations <- Moes$results
Moes_geom <- Moes_Locations$geometry
Moes_geom <- Moes_geom$location

Cinnabon_Locations <- Cinnabon$results
Cinnabon_geom <- Cinnabon_Locations$geometry
Cinnabon_geom <- Cinnabon_geom$location

Auntie_Locations <- Auntie$results
Auntie_geom <- Auntie_Locations$geometry
Auntie_geom <- Auntie_geom$location

Mcalisters_Locations <- Mcalisters$results
Mcalisters_geom <- Mcalisters_Locations$geometry
Mcalisters_geom <- Mcalisters_geom$location

Schlotzkys_Locations <- Schlotzkys$results
Schlotzkys_geom <- Schlotzkys_Locations$geometry
Schlotzkys_geom <- Schlotzkys_geom$location

#Plotting Locations of restaurants on the Atlanta maap

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = Jamba_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "darkred") + 
  geom_point(data = Moes_geom, aes(x = lng, y = lat), size = 4, 
                                                        shape = 23, fill = "darkred") +
  geom_point(data = Cinnabon_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "darkred") +
  geom_point(data = Auntie_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "darkred") +
  geom_point(data = Mcalisters_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "darkred") +
  geom_point(data = Schlotzkys_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "darkred")+
  ggtitle("Focus Brands Stores in Atlanta")


#Extracting biggest competitors' locations.
#Looking at Chick-fi-A locations

ChickFilA <- google_places(search_string = "Chick-Fil-A Atlanta, Georgia",radius=50000,  key = "Insert Google API Here")

token <- ChickFilA$next_page_token

ChickFilA_2 <- google_places("Chick-Fil-A Atlanta, Georgia",  key = "Insert Google API Here",
                             page_token = token,
                             radius = 50000)

ChickFilA_Locations <- ChickFilA$results
ChickFilA_geom <- ChickFilA_Locations$geometry
ChickFilA_geom <- ChickFilA_geom$location

ChickFilA_Locations_2 <- ChickFilA_2$results
ChickFilA_geom_2 <- ChickFilA_Locations_2$geometry
ChickFilA_geom_2 <- ChickFilA_geom_2$location

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = ChickFilA_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "blue") +
  geom_point(data = ChickFilA_geom_2, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "blue") +
  ggtitle("Chick-Fil-A Stores in Atlanta")


#Locating locations of Chipotle

Chipotle <- google_places(search_string = "Chipotle, Georgia",radius=50000,  key = "Insert Google API Here")

Chipotle_2 <-  google_places("Chipotle Atlanta, Georgia",  key = "Insert Google API Here",
                             page_token = token,
                             radius = 50000)

Chipotle_Locations <- Chipotle$results
Chipotle_geom <- Chipotle_Locations$geometry
Chipotle_geom <- Chipotle_geom$location

Chipotle_Locations_2 <- Chipotle_2$results
Chipotle_geom_2 <- Chipotle_Locations_2$geometry
Chipotle_geom_2 <- Chipotle_geom_2$location

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = Chipotle_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "Yellow") +
  geom_point(data = Chipotle_geom_2, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "Yellow") +
  ggtitle("Chipotle Stores in Atlanta")

#Examining McDonald's Locations in Atlanta

McDonalds <- google_places(search_string = "McDonalds Atlanta, Georgia",radius=50000,  key = "Insert Google API Here")

McDonalds_2 <-  google_places("McDonalds Atlanta, Georgia",  key = "Insert Google API Here",
                             page_token = token,
                             radius = 50000)

McDonalds_Locations <- McDonalds$results
McDonalds_geom <- McDonalds_Locations$geometry
McDonalds_geom <- McDonalds_geom$location

McDonalds_Locations_2 <- McDonalds_2$results
McDonalds_geom_2 <- McDonalds_Locations_2$geometry
McDonalds_geom_2 <- McDonalds_geom_2$location

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = McDonalds_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "green") +
  geom_point(data = McDonalds_geom_2, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "green") +
  ggtitle("McDonald's Stores in Atlanta")

#mapping Snacking and baked goods stores

SweetHut <- google_places(search_string = "Sweet Hut Bakery Atlanta, Georgia",radius=50000,  key = "Insert Google API Here")

SweetHut_Locations <-SweetHut$results
SweetHut_geom <- SweetHut_Locations$geometry
SweetHut_geom <- SweetHut_geom$location

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = SweetHut_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "Grey") +
  ggtitle("Sweet Hut Stores in Atlanta")

BubbleTea <- google_places(search_string = "Bubble Tea Atlanta, Georgia",radius=50000,  key = "Insert Google API Here")

BubbleTea_Locations <- BubbleTea$results
BubbleTea_geom <- BubbleTea_Locations$geometry
BubbleTea_geom <- BubbleTea_geom$location

ggmap(get_map(location = "Atlanta",maptype = "terrain", color = "color")) +
  geom_point(data = BubbleTea_geom, aes(x = lng, y = lat), size = 4, 
             shape = 23, fill = "Grey") +
  ggtitle("Bubble Tea Shops in Atlanta")
