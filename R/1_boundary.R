#-----------------------------------------------------------------------------#
#       1. Clean city boundaries and create rasters with exp grid codes       #
#-----------------------------------------------------------------------------#


pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2026_wildfires/"
# pathroot <- ""

# 1. Read ----

city03_14 <- read_sf("data/raw/boundaries/cities_2000-14.shp")
city15_24 <- read_sf("data/raw/boundaries/cities_lcde.shp")


# 2. Merge with grid ----

# Exposure boundaries
gridgeom <- rast(paste0(pathroot, "data/raw/SILAM/europePMfire_2003to2024daymean.nc4"), lyrs = 1)
expo_polys <- st_as_sf(as.polygons(gridgeom, trunc = F, dissolve = F)) %>% 
  mutate(GRD_ID = 1:n()) %>% 
  dplyr::select(GRD_ID)
expo_points <- st_centroid(expo_polys)
# Re-project city boundaries to the same CRS as the exposure grid
target_crs <- st_crs(expo_points)
city03_14 <- st_transform(city03_14, target_crs)
city15_24 <- st_transform(city15_24, target_crs)

# Extract all grid IDs that fall within a city boundary
# 2003-2014
expo_points1 <- st_join(expo_points, city03_14)
# There are cities without any grid points falling within the boundary
# for these we assign the closest point to the city centroids
cities_without_points <- st_join(city03_14, expo_points, left = T) %>% 
  filter(is.na(GRD_ID)) %>% select(URAU_CODE, geometry)
# Filter missing cities and calculate the centroid for each of the missing polygons
cities_missing<- city03_14 %>% filter(URAU_CODE%in%c(cities_without_points$URAU_CODE))
city_centroids <- st_centroid(cities_missing)
# Find the closest point from the full 'expo_points' dataset for each centroid
closest_assignments <- st_join(city_centroids, expo_points, join = st_nearest_feature)
# merge
expo_points1 <- rbind(expo_points1, closest_assignments) %>% filter(!is.na(URAU_CODE))
rm(cities_without_points, cities_missing, city_centroids, closest_assignments)

# 2015-2024
expo_points2 <- st_join(expo_points, city15_24)
# There are cities without any grid points falling within the boundary
# for these we assign the closest point to the cities centroid
cities_without_points <- st_join(city15_24, expo_points, left = T) %>% 
  filter(is.na(GRD_ID)) %>% select(URAU_CODE, geometry)
# Filter missing cities and calculate the centroid for each of the missing polygons
cities_missing<- city15_24 %>% filter(URAU_CODE%in%c(cities_without_points$URAU_CODE))
city_centroids <- st_centroid(cities_missing)
# Find the closest point from the full 'expo_points' dataset for each centroid
closest_assignments <- st_join(city_centroids, expo_points, join = st_nearest_feature)
# merge
expo_points2 <- rbind(expo_points2, closest_assignments) %>% filter(!is.na(URAU_CODE))
rm(cities_without_points, cities_missing, city_centroids, closest_assignments)


# 3. Add region IDs ---- 

euroregions <- import(paste0(pathroot, "data/raw/cities/List_cities.xlsx")) %>% 
  dplyr::select(1,3,4,6,7) 
euroregions <- euroregions[!duplicated(euroregions),]
names(euroregions) <- c("country_name", "URAU_CODE", "URAU_NAME", "region", "citysize")
# merge XXL and Global city city size categories from UN geoscheme into one category (≥XXL)
euroregions <- euroregions %>% 
  mutate(citysize = ifelse(citysize %in% c("XXL", "Global city"), "≥XXL", citysize))
expo_points1 <- left_join(expo_points1 %>% select(-c(CITY_CPTL, FUA_CODE, URAU_NAME)), 
                          euroregions, by = c("URAU_CODE" = "URAU_CODE"))
expo_points2 <- left_join(expo_points2 %>% select(-c(CITY_CPTL, FUA_CODE, URAU_NAME)), 
                          euroregions, by = c("URAU_CODE" = "URAU_CODE"))


# 4. Write ----

write_sf(expo_points1, paste0(pathroot, "data/processed/expocentroids_city_03-14.gpkg"))
write_sf(expo_points2, paste0(pathroot, "data/processed/expocentroids_city_15-24.gpkg"))

# Clean
rm(list=ls())
