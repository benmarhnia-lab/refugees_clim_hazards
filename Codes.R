########################################################################
########################################################################
#######                                                          ####### 
#######                           PROJECT:                       #######
#######                                                          ####### 
#######  Mapping climate-related hazards along migration routes  ####### 
#######                                                          ####### 
#######                                                          ####### 
########################################################################
########################################################################

# load packages
library(ggplot2)
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(tidyverse)
library(raster)
library(geodata)
library(sf)
library(countrycode)
library(rnaturalearth)
library(dplyr)
library(sp)
library(colorspace)
hcl_palettes(plot = TRUE)
options(scipen=999)
library(sjPlot)
library(ggspatial)
library(zoo)

setwd("G:/My Drive/")

# Set the Google API key: 
# this sets your google map permanently
register_google(key = XXX, write=TRUE)


###############################################################################
## Plot the transition locations
###############################################################################

## Upload the refugee data
ref_data <- read.csv("./Data_refugees.csv")
colnames(ref_data)[1] = "PID"

ref_data <- ref_data[c(1:67),]

## Add geographical information and convert to spatial points
home <- ref_data %>% 
  dplyr::select(PID, Home.Location) %>% 
  rename(Location = Home.Location) %>% 
  mutate(descr = "Home") 

## First stop
stop1 <- ref_data %>% 
  dplyr::select(PID, First.Stop.Location) %>% 
  rename(Location = First.Stop.Location) %>% 
  mutate(descr = "First stop") 

## Second stop
stop2 <- ref_data %>% 
  dplyr::select(PID, Second.Stop.Location) %>% 
  rename(Location = Second.Stop.Location) %>% 
  mutate(descr = "Second stop") 

## Third Stop
stop3 <- ref_data %>% 
  dplyr::select(PID, Third.Stop.Location) %>% 
  rename(Location = Third.Stop.Location) %>% 
  mutate(descr = "Third stop") 

points_geo <- rbind(home, stop1, stop2, stop3)
points_geo <- points_geo %>% 
  #cannot find a camp called Albashabshy
  mutate(Location = ifelse(Location == "Albashabshy Camp, Jordan", "N/A", Location)) %>%
  mutate(Location = ifelse(Location == "Zaatari Camp, Jordan", "Zaatari refugee camp, Jordan", Location)) %>%
  mutate(Location = ifelse(Location == "Sed el Bauchrieh, Beirut, Lebanon", "Beirut, Lebanon", Location)) %>%
  mutate(Location = ifelse(Location == "Sed el Bauchrieh, Jdeideh, Lebanon", "Beirut, Lebanon", Location)) %>%
  mutate(Location = ifelse(Location == "Dekwaneh, Lebanon", "Beirut, Lebanon", Location)) %>%
  mutate(Location = ifelse(Location == "Bouchrieh, Lebanon", "Beirut, Lebanon", Location)) %>%
  mutate(Location = ifelse(Location == "Jaramana, Syria", "Damascus, Syria", Location)) %>%
  mutate(Location = ifelse(Location == "Barzeh, Syria", "Damascus, Syria", Location)) %>%
  #google maps does not detect town called Khala in Syria but google suggests it's in Hala area
  mutate(Location = ifelse(Location == "Khala, Syria", "Hama, Syria", Location)) %>%
  mutate_geocode(Location) %>%
  #correct the location for Zaatari
  mutate(lat = ifelse(Location == "Zaatari refugee camp, Jordan", 32.29, lat )) %>% 
  mutate(lon = ifelse(Location == "Zaatari refugee camp, Jordan", 36.34, lon )) %>% 
  #remove observations without specific location for the mapping
  filter(Location != "Syria" & Location != "Jordan" & Location != "United States" & Location != "Lebanon" & Location != "Turkey" & Location != "Egypt" & Location != "Sweden" & Location != "San Diego, CA"& Location != "Iraq" & Location != "Nebraska") %>%
  #not sure what this location is
  filter(Location != "Muhafathat Naynawah, Iraq") %>% 
  #add city names
  mutate(city = gsub(",.*$", "", Location)) %>% 
  mutate(country = gsub(".*\\,", "", Location))

points_geo <- points_geo %>% 
  na.omit() 

points_geo <- points_geo %>% 
  mutate(Description = ifelse(descr == "Home", "Home", "Transition"))%>% 
  group_by(Location, lon, lat, city, Description) %>% 
  summarize(Refugees = n()) %>% 
  ungroup() 

data_panel_B_transition_locations <- points_geo
write.csv(data_panel_B_transition_locations, "data_panel_B_transition_locations.csv")

## Convert to spatial points
points_geo <- st_as_sf(points_geo, 
                     coords = c(x = "lon", y = "lat"), 
                     crs = 4326) #assign crs

## Subset to transition locations only
#points_geo <- points_geo %>% 
#  filter(Description=="Transition")

## Add home and destination admin 1 information (to be used to link with EMDAT data)
list = c("Syria", "Iraq", "Turkey", "Jordan", "Lebanon", "Egypt")
countrycode(list, origin = 'country.name', destination = 'iso3c')

admin_syr <- gadm(country="SYR", level=1, path=tempdir(), version="latest")
admin_irq <- gadm(country="IRQ", level=1, path=tempdir(), version="latest")
admin_tur <- gadm(country="TUR", level=1, path=tempdir(), version="latest")
admin_jor <- gadm(country="JOR", level=1, path=tempdir(), version="latest")
admin_lbn <- gadm(country="LBN", level=1, path=tempdir(), version="latest")
admin_egy <- gadm(country="EGY", level=1, path=tempdir(), version="latest")

admin <- rbind(admin_syr, admin_irq, admin_tur, admin_jor, admin_lbn, admin_egy) 
admin.df <- as.data.frame(admin)
admin <- sf::st_as_sf(admin)
projection(admin)
admin_syr <- sf::st_as_sf(admin_syr)
admin_irq <- sf::st_as_sf(admin_irq)

class(admin)
class(points_geo)

## Link the spatial point and spatial polygons
points_admin <- st_join(points_geo, admin, join = st_within) %>% 
  dplyr::select(Location, city, COUNTRY, NAME_1, geometry) 

## Load the world map for background
worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')

## Transition and home countries
worldmap_reduced <- worldmap %>% 
  filter(name %in% c("Syria", "Iraq", "Turkey", "Jordan", "Lebanon", "Egypt", "Sweden"))

## Transition countries  
worldmap_reduced_1 <- worldmap %>% 
  filter(name %in% c("Turkey", "Jordan", "Lebanon", "Egypt", "Sweden"))

## Home countries
worldmap_reduced_2 <- worldmap %>% 
  filter(name %in% c("Syria", "Iraq"))

map1 <- ggplot() +
  geom_sf(data = worldmap, fill = "#dfe9ee", color = "white") +
  geom_sf(data = worldmap_reduced_1, fill = "#9bd0d1", color = "darkgray", size=0.2) +
  geom_sf(data = worldmap_reduced_2, fill = "#e7d2ba", color = "darkgray", size=0.2) +
  geom_sf(data = points_geo, fill = NA, aes(size = factor(Refugees), color=Description), alpha = 0.75) +
  scale_color_manual(values=c("#ed7c1c", "#ed1c24")) +
  geom_sf_text(data = points_geo, aes(label = city), position=position_dodge(width = 0.9), 
               vjust=-1, size = 3.5) +
  geom_sf_text(data = worldmap_reduced, aes(label = name), color = "#7c7f82", size = 5) +
  coord_sf(xlim = c(20, 52), ylim = c(25, 45), expand = FALSE) +
  annotation_scale() +
  theme_void()

save_plot("./figures/map1.svg", fig = map1, width=20, height=20)


###############################################################################
## Plot the migration journeys via flow chart
###############################################################################

flow_data <- rbind(home, stop1, stop2, stop3)
flow_data <- flow_data %>% 
  mutate(Location = ifelse(Location %in% c("Zakho, Iraq", "Baghdad, Iraq", "Basra, Iraq", "Mosul, Iraq", "Kirkuk, Iraq", "Irbil, Iraq", "Muhafathat Naynawah, Iraq", "Telskuf, Iraq", "Tall kayf, Iraq"), "Iraq", Location)) %>% 
  mutate(Location = ifelse(Location %in% c("Jaramana, Syria", "Qamishli, Syria", "Barzeh, Syria", "Al Zabadani, Syria", "Damascus, Syria", "Daraa, Syria", "Khala, Syria", "Homs, Syria"), "Syria", Location)) %>% 
  mutate(Location = ifelse(Location %in% c("Albashabshy Camp, Jordan" , "Zaatari Camp, Jordan", "Amman, Jordan", "Irbid, Jordan"), "Jordan", Location)) %>% 
  mutate(Location = ifelse(Location %in% c("Corum, Turkey" , "Ankara, Turkey", "Canakkale, Turkey" , "Kirsehir, Turkey", "Afyonkarahisar, Turkey" ,"Yalova, Turkey", "Istanbul, Turkey"), "Turkey", Location)) %>%
  mutate(Location = ifelse(Location %in% c("Sed el Bauchrieh, Beirut, Lebanon" , "Dekwaneh, Lebanon", "Bouchrieh, Lebanon",  "Sed el Bauchrieh, Jdeideh, Lebanon", "Beirut, Lebanon"), "Lebanon", Location)) %>% 
  mutate(Location = ifelse(Location %in% c("Cairo, Egypt"), "Egypt", Location)) %>%
  mutate(Location = ifelse(Location %in% c("San Diego, CA", "Nebraska"), "United States", Location))

flow_data <- reshape(flow_data, idvar = "PID", timevar = "descr", direction = "wide")
str(flow_data)
colnames(flow_data)[2] = "Home"
colnames(flow_data)[3] = "First.Stop"
colnames(flow_data)[4] = "Second.Stop"
colnames(flow_data)[5] = "Third.Stop"

flow_data <- flow_data %>% 
  group_by(Home, First.Stop, Second.Stop, Third.Stop) %>% 
  summarize(Refugees = n()) %>% 
  ungroup()

flow_data <- flow_data %>% 
  mutate_geocode(Departure) %>% 
  rename(Departure.lon = lon,
         Departure.lat = lat) %>% 
  mutate_geocode(Destination) %>% 
  rename(Destination.lon = lon,
         Destination.lat = lat) %>% 
  mutate(Destination.lon = ifelse(Destination == "United States", -17, Destination.lon)) %>% 
  mutate(Destination.lat = ifelse(Destination == "United States", 40, Destination.lat)) %>% 
  mutate(Departure.lon = ifelse(Departure == "Jordan", 36.20, Departure.lon)) %>% 
  mutate(Departure.lat = ifelse(Departure == "Jordan", 30.57, Departure.lat)) %>% 
  mutate(Destination.lon = ifelse(Destination == "Jordan", 36.20, Destination.lon)) %>% 
  mutate(Destination.lat = ifelse(Destination == "Jordan", 30.57, Destination.lat)) %>% 
  na.omit()   

data_panel_A_migration_routes <- flow_data
write.csv(data_panel_A_migration_routes, "data_panel_A_migration_routes.csv")

map2 <- ggplot() + 
  geom_sf(data = worldmap, fill = "#dfe9ee", color = "white") +
  geom_sf(data = worldmap_reduced_1, fill = "#9bd0d1", color = "darkgray", size=0.2) +
  geom_sf(data = worldmap_reduced_2, fill = "#e7d2ba", color = "darkgray", size=0.2) +  #coord_map(projection = "albers", lat0 = 39, lat1 = 45,
  geom_curve(data = flow_data, aes(x = Departure.lon, y = Departure.lat, xend = Destination.lon, yend = Destination.lat, 
                                   linewidth=n), color="#bd0026", alpha = 1, curvature = -0.2, #lineend="round", 
             arrow = arrow(length = unit(0.15, "cm"), type="closed"),  linejoin="mitre", lineend="round") + #
  scale_colour_distiller(palette="Reds", name="Frequency", guide = "colorbar") +
  geom_sf_text(data = worldmap_reduced, aes(label = name), color = "#7c7f82", size = 4) +
  coord_sf(xlim = c(-20, 52), ylim = c(20, 65), expand = FALSE) +
  annotation_scale() +
  theme_void() 

save_plot("map2.svg", fig = map2, width=30, height=30)

###############################################################################
## Plot the number of disasters per admin 1 area (data source: EMDAT)
###############################################################################

load("./pend-gdis-1960-2018-disasterlocations-rdata/pend-gdis-1960-2018-disasterlocations.Rdata")
str(GDIS_disasterlocations)
class(GDIS_disasterlocations)

GDIS_disasterlocations <- GDIS_disasterlocations %>% 
  filter(country %in% c("Syria", "Iraq", "Turkey", "Jordan", "Lebanon", "Egypt", "Sweden"))

GDIS <- GDIS_disasterlocations %>% 
  dplyr::select(country, adm1, id, disastertype, disasterno) %>% 
  mutate(country = ifelse(country == "Syria", "Syrian Arab Republic", country))

unique(GDIS$country)

## Load the EMDAT data
emdat <- read.csv("emdat_new.csv")
unique(emdat$Country)

emdat <- emdat %>% 
  filter(Disaster.Subgroup == "Hydrological" | Disaster.Subgroup == "Meteorological" | Disaster.Subgroup == "Climatological") %>% 
  dplyr::select(c(Dis.No, Country, Year, Disaster.Group, Disaster.Subgroup, Disaster.Type, Disaster.Subtype, Disaster.Subsubtype, Event.Name, Location, Start.Year, Start.Month, Start.Day, End.Year, End.Month, End.Day, Geo.Locations, Total.Affected)) %>% 
  mutate(Dis.No = str_sub(Dis.No, end = -5)) 

emdat <- emdat %>% 
  filter(Dis.No %in% GDIS$disasterno)

GDIS <- GDIS %>% 
  filter(disasterno %in% emdat$Dis.No)

## Reduce polygons to admin level 1 only
GDIS_union <- st_cast(GDIS$geometry, "POLYGON")

GDIS_union = GDIS_union %>%
  st_sf %>%
  st_cast

GDIS_sp <- sf:::as_Spatial(GDIS)
GDIS <- GDIS %>% st_drop_geometry()
GDIS <- cbind(GDIS, GDIS_union)
GDIS <-  GDIS %>% left_join(emdat, by=c("country"="Country", "disasterno" = "Dis.No"))
  
GDIS_tot <- GDIS %>% group_by(country, adm1, geometry) %>% summarise(disasters = n())
st_geometry(GDIS_tot) <- GDIS_tot$geometry

class(GDIS_tot)

cntr_bound <- admin %>% 
  group_by(COUNTRY) %>% 
  summarize(geometry = st_union(geometry))

map2 <- ggplot() +
  geom_sf(data = worldmap, fill = "#dfe9ee", color = "white") +
  geom_sf(data = admin, fill = "darkgray", size = 0.1, colour = "white") +
  geom_sf(data = GDIS_tot,  colour = "white", aes(fill = as.factor(disasters))) +
  geom_sf(data = cntr_bound, fill = NA, color = "darkgray", size=0.6) +
  scale_fill_brewer(palette="RdPu", na.translate = F) + 
  labs(fill = "Number of disasters") +
  #geom_sf(data = points_geo, fill = NA, color="#4deeea", aes(size = factor(n)), alpha = 0.5) +
  geom_sf(data = points_geo, shape=4, col = "black") +
  guides(color = "none") + 
  coord_sf(xlim = c(20, 52), ylim = c(20, 45), expand = FALSE) +
  annotation_scale() +
  theme_void() 

#save_plot("map2.png", fig = map3, width=20, height=20)
save_plot("map2.svg", fig = map2, width=20, height=20)

data_panel_C_disasters_by_admin <- sf::st_drop_geometry(GDIS_tot)
write.csv(data_disasters_by_admin, "data_panel_C_disasters_by_admin1.csv")

###############################################################################
## Plot the number of disasters per country
###############################################################################

emdat <- emdat %>% mutate(Start.Month = ifelse(is.na(Start.Month), 1, Start.Month))
emdat$Date <- as.yearmon(paste(emdat$Start.Year, emdat$Start.Month), "%Y %m")
emdat$Date2 = as.Date(emdat$Date) 
emdat_trend <- emdat %>% 
  #mutate(Total.Affected = ifelse(is.na(Total.Affected), 0, Total.Affected)) %>% 
  filter(!is.na(Total.Affected)) %>% 
  group_by(Country, Date, Date2, Disaster.Type) %>% 
  summarise(n = n(), Affected = sum(Total.Affected, na.rm=F))


emdat_by_country <- emdat_trend %>% 
  group_by(Country, Disaster.Type) %>%
  summarise(Disasters = n())
  
emdat_by_country$Country <- factor(emdat_by_country$Country , levels=c("Turkey", "Egypt", "Iraq", "Lebanon", "Syrian Arab Republic", "Jordan"))
emdat_by_country$Disaster.Type <- factor(emdat_by_country$Disaster.Type , levels=c("Drought", "Extreme temperature", "Landslide", "Storm", "Flood"))

plot1 <- ggplot(data=emdat_by_country, aes(x=Disasters, y=Country, fill = Disaster.Type)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("#fddc5e", "#d35158", "#90b791", "#46337e", "#5b7fb7")) +
  theme_classic() +
  labs(fill="Disaster type") +
  xlab("Number of disasters") 

save_plot("plot1.svg", fig = plot1, width=15, height=10)

save.image("Data.RData") 

data_panel_D_disasters_by_country <- emdat_by_country
write.csv(data_disasters_by_country, "data_panel_D_disasters_by_country.csv")

