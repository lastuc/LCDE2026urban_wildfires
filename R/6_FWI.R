#-----------------------------------------------------------------------------#
#                          6. Process fire danger                             #
#-----------------------------------------------------------------------------#

library(data.table)
library(here)
library(terra)
library(tidyverse)


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDEUrban2024_wildfires/"
pathroot <- ""

# exposure blueprint
exposure <- rast(paste0(pathroot, "data/raw/SILAM/europePMfire_2003to2024daymean.nc4"))[[1]]

# raw FWI file, global 0.5º resolution daily
danger <- rast("data/raw/FWI/FWI_era5_glob_0_5_1979_2024.nc4") 
dangertimes <- year(time(danger))

# extraction points
pointcity1 <- vect(paste0(pathroot, "data/processed/expocentroids_city_03-14.gpkg"))
pointcity2 <- vect(paste0(pathroot, "data/processed/expocentroids_city_15-24.gpkg"))
dfcity1 <- as.data.frame(pointcity1)
dfcity2 <- as.data.frame(pointcity2)

# population data
pop_tot <- fread("data/raw/population/LCDE_prep/cities_population_lcde.csv") %>% 
  select(1:3,7) %>% rename(year=Year) %>% setDT


# run periods separately due to change in city boundaries
# 2003-2014
# create empty lists
allFWI_city <- list()
allFWI_size <- list()
allFWI_country <- list()
allFWI_region <- list()
allFWI_euro <- list()


for(y in 2003:2014){

  print(y)

  # Read annual data, crop and average
  yFWI <- rast(paste0(pathroot, "data/raw/FWI/FWI_era5_glob_0_5_1979_2024.nc4"),
                     lyrs = dangertimes == y)
  yFWI <- rotate(yFWI)
  crs(yFWI)  <- "epsg:4326"
  yFWI <- crop(yFWI, exposure)
  yFWI <- app(yFWI, "mean")

  # Re-sample at the exposure geometries, extract
  yFWI <- resample(yFWI, exposure, method = "bilinear")
  names(yFWI) <- "FWI"
  yFWI <- terra::extract(yFWI, pointcity1)[-1]

  # Merge
  ydata <- cbind(dfcity1, yFWI)
  ydata <- setDT(ydata)
  ydata <- ydata[!is.na(FWI)]
  
  
  # JOIN annual POPULATION
  ypop_tot <- pop_tot[pop_tot$year == y, ]
  # Merge the datasets
  # Join the daily data with the annual population data based on URAU_CODE.
  # This assigns the annual population to each row.
  ydata_pop <- merge(ypop_tot, ydata, by = "URAU_CODE")
  # Calculate the number of grids per URAU_CODE
  # Group by date and URAU_CODE to count the number of unique grids each day.
  ydata_pop[, num_grids_year := .N, by = .(year, URAU_CODE)]
  # Distribute the population equally to each grid
  # Divide the total population of the URAU_CODE by the number of grids present on that day.
  ydata <- ydata_pop[, pop := pop_total / num_grids_year]
  
  # Population and spatial averages: city
  FWI_city <- copy(ydata)
  FWI_city[, popw := pop/sum(pop), by = .(URAU_CODE)]
  FWI_city <- FWI_city[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                           by = .(URAU_CODE)]
  FWI_city$year <- y
  setDF(FWI_city)

  # Population and spatial averages: city size
  FWI_size <- copy(ydata)
  FWI_size[, popw := pop/sum(pop), by = .(citysize)]
  FWI_size <- FWI_size[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                   by = .(citysize)]
  FWI_size$year <- y
  setDF(FWI_size)
  
  # Population and spatial averages: country
  FWI_country <- copy(ydata)
  FWI_country[, popw := pop/sum(pop), by = .(CNTR_CODE)]
  FWI_country <- FWI_country[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                         by = .(CNTR_CODE)]
  FWI_country$year <- y
  setDF(FWI_country)

  # Population and spatial averages: region
  FWI_region <- copy(ydata)
  FWI_region[, popw := pop/sum(pop), by = .(region)]
  FWI_region <- FWI_region[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                         by = .(region)]
  FWI_region$year <- y
  setDF(FWI_region)
  
  # Population and spatial averages: Europe
  FWI_euro <- copy(ydata)
  FWI_euro[, popw := pop/sum(pop)]
  FWI_euro <- FWI_euro[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI))]
  FWI_euro$year <- y
  setDF(FWI_euro)

  # Store and clean
  allFWI_city[[as.character(y)]] <- FWI_city
  allFWI_size[[as.character(y)]] <- FWI_size
  allFWI_country[[as.character(y)]] <- FWI_country
  allFWI_region[[as.character(y)]] <- FWI_region
  allFWI_euro[[as.character(y)]] <- FWI_euro
  rm("FWI_city", "FWI_size", "FWI_country", "FWI_region", "FWI_euro", 
     "ydata", "yFWI")

}


# 2015-2023
# create empty lists
allFWI_city2 <- list()
allFWI_size2 <- list()
allFWI_country2 <- list()
allFWI_region2 <- list()
allFWI_euro2 <- list()


for(y in 2015:2023){
  
  print(y)
  
  # Read annual data, crop and average
  yFWI <- rast(paste0(pathroot, "data/raw/FWI/FWI_era5_glob_0_5_1979_2024.nc4"),
               lyrs = dangertimes == y)
  yFWI <- rotate(yFWI)
  crs(yFWI)  <- "epsg:4326"
  yFWI <- crop(yFWI, exposure)
  yFWI <- app(yFWI, "mean")
  
  # Re-sample at the exposure geometries, extract
  yFWI <- resample(yFWI, exposure, method = "bilinear")
  names(yFWI) <- "FWI"
  yFWI <- terra::extract(yFWI, pointcity2)[-1]
  
  # Merge
  ydata <- cbind(dfcity2, yFWI)
  ydata <- setDT(ydata)
  ydata <- ydata[!is.na(FWI)]
  
  # JOIN annual POPULATION
  ypop_tot <- pop_tot[pop_tot$year == y, ]
  # Merge the datasets
  # Join the daily data with the annual population data based on URAU_CODE.
  # This assigns the annual population to each row.
  ydata_pop <- merge(ypop_tot, ydata, by = "URAU_CODE")
  # Calculate the number of grids per URAU_CODE
  # Group by date and URAU_CODE to count the number of unique grids each day.
  ydata_pop[, num_grids_year := .N, by = .(year, URAU_CODE)]
  # Distribute the population equally to each grid
  # Divide the total population of the URAU_CODE by the number of grids present on that day.
  ydata <- ydata_pop[, pop := pop_total / num_grids_year]
  
  # Population and spatial averages: city
  FWI_city <- copy(ydata)
  FWI_city[, popw := pop/sum(pop), by = .(URAU_CODE)]
  FWI_city <- FWI_city[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                       by = .(URAU_CODE)]
  FWI_city$year <- y
  setDF(FWI_city)
  
  # Population and spatial averages: city size
  FWI_size <- copy(ydata)
  FWI_size[, popw := pop/sum(pop), by = .(citysize)]
  FWI_size <- FWI_size[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                       by = .(citysize)]
  FWI_size$year <- y
  setDF(FWI_size)
  
  # Population and spatial averages: country
  FWI_country <- copy(ydata)
  FWI_country[, popw := pop/sum(pop), by = .(CNTR_CODE)]
  FWI_country <- FWI_country[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                             by = .(CNTR_CODE)]
  FWI_country$year <- y
  setDF(FWI_country)
  
  # Population and spatial averages: region
  FWI_region <- copy(ydata)
  FWI_region[, popw := pop/sum(pop), by = .(region)]
  FWI_region <- FWI_region[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI)),
                           by = .(region)]
  FWI_region$year <- y
  setDF(FWI_region)
  
  # Population and spatial averages: Europe
  FWI_euro <- copy(ydata)
  FWI_euro[, popw := pop/sum(pop)]
  FWI_euro <- FWI_euro[, .(FWI_pop = sum(popw*FWI), FWI_spatial = mean(FWI))]
  FWI_euro$year <- y
  setDF(FWI_euro)
  
  # Store and clean
  allFWI_city2[[as.character(y)]] <- FWI_city
  allFWI_size2[[as.character(y)]] <- FWI_size
  allFWI_country2[[as.character(y)]] <- FWI_country
  allFWI_region2[[as.character(y)]] <- FWI_region
  allFWI_euro2[[as.character(y)]] <- FWI_euro
  rm("FWI_city", "FWI_size", "FWI_country", "FWI_region", "FWI_euro", 
     "ydata", "yFWI")
  
}


# Save to disk
allFWI_city3 <- c(allFWI_city, allFWI_city2)
allFWI_city4 <- do.call(rbind, allFWI_city3)
write_csv(allFWI_city4, "data/processed/FWI_city.csv")
allFWI_size3 <- c(allFWI_size, allFWI_size2)
allFWI_size4 <- do.call(rbind, allFWI_size3)
write_csv(allFWI_size4, "data/processed/FWI_citysize.csv")
allFWI_country3 <- c(allFWI_country, allFWI_country2)
allFWI_country4 <- do.call(rbind, allFWI_country3)
write_csv(allFWI_country4, "data/processed/FWI_country.csv")
allFWI_region3 <- c(allFWI_region, allFWI_region2)
allFWI_region4 <- do.call(rbind, allFWI_region3)
write_csv(allFWI_region4, "data/processed/FWI_region.csv")
allFWI_euro3 <- c(allFWI_euro, allFWI_euro2)
allFWI_euro4 <- do.call(rbind, allFWI_euro3)
write_csv(allFWI_euro4, "data/processed/FWI_euro.csv")

# clean
rm(list = ls())
