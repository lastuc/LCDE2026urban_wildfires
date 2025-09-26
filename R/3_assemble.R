#----------------------------------------------#
#         3. Extract daily exposures           #
#----------------------------------------------#


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDEUrban2026_wildfires/"
pathroot <- ""



# 1. Assign areas, exposure by grid of population for each day ----

# Read data
pointcity1 <- vect(paste0(pathroot, "data/processed/expocentroids_city_03-14.gpkg"))
pointcity2 <- vect(paste0(pathroot, "data/processed/expocentroids_city_15-24.gpkg"))
dfcity1 <- as.data.frame(pointcity1)
dfcity2 <- as.data.frame(pointcity2)
# population <- rast(paste0(pathroot, "data/processed/population_yearly.tif"))
# population <- subset(population, names(population) != "year")
# names(population) <- as.character(c(2003:2024))
pop_tot <- read_csv("data/raw/population/LCDE_prep/cities_population_lcde.csv") %>% 
  select(1:3,7) %>% filter(Year>2002) %>% rename(year=Year) %>% setDT

# Create date sequence
dayseq <- seq.Date(as.Date("2003-01-01"), as.Date("2023-12-31"), by = "1 day")

# Extract geographical IDs, SILAM for each day-cell per year, population
# Run it by periods due to city boundary change
# 2003-2014
for(y in 2003:2014){

  print(y)

  # Extract SILAM ----
  yindex <- year(dayseq) == y
  ysilam <- rast(paste0(pathroot, "data/raw/SILAM/europePMfire_2003to2024daymean.nc4"), lyrs = yindex)
  names(ysilam) <- as.character(as.Date(time(ysilam)))
  ysilam <- ysilam*1e+9 # change of units
  ysilam <- terra::extract(ysilam, pointcity1)[-1]

  # Merge, reformat and write----
  ydata <- cbind(dfcity1, ysilam)
  ydata <- tidyr::pivot_longer(ydata,
                               cols = -c("GRD_ID","URAU_CODE","URAU_NAME","NUTS3_2021",
                                         "CNTR_CODE","country_name","region","citysize",
                                         # "population",
                                         "AREA_SQM"),
                               names_to = "date", values_to = "pm25")
  
  # JOIN annual POPULATION
  ypop_tot <- pop_tot[year == y]
  
  # Merge the datasets
  # Join the daily data with the annual population data based on URAU_CODE.
  # This assigns the annual population to each row.
  ydata_pop <- merge(ypop_tot, ydata, by = "URAU_CODE")
  
  # Calculate the number of grids per URAU_CODE 
  # Group by date and URAU_CODE to count the number of unique grids each day.
  ydata_pop[, num_grids_daily := .N, by = .(date, URAU_CODE)]
  
  # Distribute the population equally to each grid
  # Divide the total population of the URAU_CODE by the number of grids present on that day.
  ydata_pop[, pop := pop_total / num_grids_daily]
  
  # DROP cities with no exposure (cities on ISLANDS in ES and PT): 
  # ES008C, ES024C, ES027C, ES055C, ES057C, ES096C, PT007C
  ydata_pop <- ydata_pop %>% filter(!is.na(pm25))
  
  readr::write_csv(ydata_pop, paste0(pathroot, "data/processed/assembled/data_", y,".csv"))
  print("saved")
  rm("ypop", "ydata", "ypop_tot", "yindex", "ysilam", "ydata_pop")
}


# 2015-2023
for(y in 2015:2023){
  
  print(y)
  
  # Extract SILAM ----
  yindex <- year(dayseq) == y
  ysilam <- rast(paste0(pathroot, "data/raw/SILAM/europePMfire_2003to2024daymean.nc4"), lyrs = yindex)
  names(ysilam) <- as.character(as.Date(time(ysilam)))
  ysilam <- ysilam*1e+9 # change of units
  ysilam <- terra::extract(ysilam, pointcity2)[-1]
  
  # Merge, reformat and write----
  ydata <- cbind(dfcity2, ysilam)
  ydata <- tidyr::pivot_longer(ydata,
                               cols = -c("GRD_ID","URAU_CODE","URAU_NAME","NUTS3_2021",
                                         "CNTR_CODE","country_name","region","citysize",
                                         # "population", 
                                         "AREA_SQM"),
                               names_to = "date", values_to = "pm25")
 
   # JOIN annual POPULATION
  ypop_tot <- pop_tot[year == y]
  
  # Merge the datasets
  # Join the daily data with the annual population data based on URAU_CODE.
  # This assigns the annual population to each row.
  ydata_pop <- merge(ypop_tot, ydata, by = "URAU_CODE")
  
  # Calculate the number of grids per URAU_CODE
  # Group by date and URAU_CODE to count the number of unique grids each day.
  ydata_pop[, num_grids := .N, by = .(date, URAU_CODE)]
  
  # Distribute the population equally to each grid
  # Divide the total population of the URAU_CODE by the number of grids present on that day.
  ydata_pop[, pop := pop_total / num_grids]
  
  # DROP cities with no exposure (cities on ISLANDS in ES and PT): 
  # ES008C, ES024C, ES027C, ES055C, ES057C, ES096C, PT007C
  ydata_pop <- ydata_pop %>% filter(!is.na(pm25))
  
  readr::write_csv(ydata_pop, paste0(pathroot, "data/processed/assembled/data_", y,".csv"))
  print("saved")
  rm("ypop", "ydata", "ypop_tot", "yindex", "ysilam", "ydata_pop")
}

rm(pointcity1, pointcity2, dfcity1, dfcity2)



# 2. Aggregate population counts ----

pop <- function(expofile){
  
  expodata <- read_csv(expofile)
  expodata <- expodata[c("URAU_CODE", "citysize", "CNTR_CODE", "region", "date", "pop")]
  
  setDT(expodata)
  
  list(
    "population_city" = expodata[, .(population = sum(pop), year = year(date)), by = .(URAU_CODE, date)][, .SD[1], by = .(year, URAU_CODE)][, date := NULL],
    "population_citysize" = expodata[, .(population = sum(pop), year = year(date)), by = .(citysize, date)][, .SD[1], by = .(year, citysize)][, date := NULL],
    "population_country" = expodata[, .(population = sum(pop), year = year(date)), by = .(CNTR_CODE, date)][, .SD[1], by = .(year, CNTR_CODE)][, date := NULL],
    "population_region" = expodata[, .(population = sum(pop), year = year(date)), by = .(region, date)][, .SD[1], by = .(year, region)][, date := NULL],
    "population_euro" = expodata[, .(population = sum(pop), year = year(date)), by = date][, .SD[1], by = year][, date := NULL]
  )
  
}

# Execute  
pop03 <- pop(paste0(pathroot, "data/processed/assembled/data_2003.csv"))
pop04 <- pop(paste0(pathroot, "data/processed/assembled/data_2004.csv"))
pop05 <- pop(paste0(pathroot, "data/processed/assembled/data_2005.csv"))
pop06 <- pop(paste0(pathroot, "data/processed/assembled/data_2006.csv"))
pop07 <- pop(paste0(pathroot, "data/processed/assembled/data_2007.csv"))
pop08 <- pop(paste0(pathroot, "data/processed/assembled/data_2008.csv"))
pop09 <- pop(paste0(pathroot, "data/processed/assembled/data_2009.csv"))
pop10 <- pop(paste0(pathroot, "data/processed/assembled/data_2010.csv"))
pop11 <- pop(paste0(pathroot, "data/processed/assembled/data_2011.csv"))
pop12 <- pop(paste0(pathroot, "data/processed/assembled/data_2012.csv"))
pop13 <- pop(paste0(pathroot, "data/processed/assembled/data_2013.csv"))
pop14 <- pop(paste0(pathroot, "data/processed/assembled/data_2014.csv"))
pop15 <- pop(paste0(pathroot, "data/processed/assembled/data_2015.csv"))
pop16 <- pop(paste0(pathroot, "data/processed/assembled/data_2016.csv"))
pop17 <- pop(paste0(pathroot, "data/processed/assembled/data_2017.csv"))
pop18 <- pop(paste0(pathroot, "data/processed/assembled/data_2018.csv"))
pop19 <- pop(paste0(pathroot, "data/processed/assembled/data_2019.csv"))
pop20 <- pop(paste0(pathroot, "data/processed/assembled/data_2020.csv"))
pop21 <- pop(paste0(pathroot, "data/processed/assembled/data_2021.csv"))
pop22 <- pop(paste0(pathroot, "data/processed/assembled/data_2022.csv"))
pop23 <- pop(paste0(pathroot, "data/processed/assembled/data_2023.csv"))


# Population by city
pop_city <- bind_rows(pop03[[1]], pop04[[1]], pop05[[1]], pop06[[1]], pop07[[1]],
                       pop08[[1]], pop09[[1]], pop10[[1]], pop11[[1]], pop12[[1]],
                       pop13[[1]], pop14[[1]], pop15[[1]], pop16[[1]], pop17[[1]],
                       pop18[[1]], pop19[[1]], pop20[[1]], pop21[[1]], pop22[[1]], 
                       pop23[[1]])
write_csv(pop_city, paste0(pathroot, "data/processed/population_city.csv"))

# Population by city size
pop_citysize <- bind_rows(pop03[[2]], pop04[[2]], pop05[[2]], pop06[[2]], pop07[[2]],
                          pop08[[2]], pop09[[2]], pop10[[2]], pop11[[2]], pop12[[2]],
                          pop13[[2]], pop14[[2]], pop15[[2]], pop16[[2]], pop17[[2]],
                          pop18[[2]], pop19[[2]], pop20[[2]], pop21[[2]], pop22[[2]], 
                          pop23[[2]])
write_csv(pop_citysize, paste0(pathroot, "data/processed/population_citysize.csv"))

# Population by country
pop_country <- bind_rows(pop03[[3]], pop04[[3]], pop05[[3]], pop06[[3]], pop07[[3]],
                         pop08[[3]], pop09[[3]], pop10[[3]], pop11[[3]], pop12[[3]],
                         pop13[[3]], pop14[[3]], pop15[[3]], pop16[[3]], pop17[[3]],
                         pop18[[3]], pop19[[3]], pop20[[3]], pop21[[3]], pop22[[3]], 
                         pop23[[3]])
write_csv(pop_country, paste0(pathroot, "data/processed/population_country.csv"))

# Population by region
pop_region <- bind_rows(pop03[[4]], pop04[[4]], pop05[[4]], pop06[[4]], pop07[[4]],
                     pop08[[4]], pop09[[4]], pop10[[4]], pop11[[4]], pop12[[4]],
                     pop13[[4]], pop14[[4]], pop15[[4]], pop16[[4]], pop17[[4]],
                     pop18[[4]], pop19[[4]], pop20[[4]], pop21[[4]], pop22[[4]], 
                     pop23[[4]])
write_csv(pop_region, paste0(pathroot, "data/processed/population_region.csv"))

# Population by Europe
pop_euro <- bind_rows(pop03[[5]], pop04[[5]], pop05[[5]], pop06[[5]], pop07[[5]],
                       pop08[[5]], pop09[[5]], pop10[[5]], pop11[[5]], pop12[[5]],
                       pop13[[5]], pop14[[5]], pop15[[5]], pop16[[5]], pop17[[5]],
                       pop18[[5]], pop19[[5]], pop20[[5]], pop21[[5]], pop22[[5]],
                       pop23[[5]])
write_csv(pop_euro, paste0(pathroot, "data/processed/population_euro.csv"))


# Clean
rm(list = ls())
