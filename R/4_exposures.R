#-----------------------------------------------------------------------------#
#                4. Population-weighted and spatial exposures                 #
#-----------------------------------------------------------------------------#


pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDEUrban2026_wildfires/"
# pathroot <- ""


# vector for PM2.5 percentiles across entire period 2003-2023 daily observations
# this has been calculated after runing this script first
perc <- c(0.03766824, 0.13392309,0.41056107, 0.80399332, 2.73855073, 10.84405124)
names(perc) <- c("perc50", "perc75", "perc90", "perc95", "perc99", "perc99.9")



# 1. Exposure Function ----

expo_aggr <- function(expofile, yearint){

  expodata <- setDT(fread(expofile)) 
  expodata <- expodata %>%
    mutate(across(c(URAU_CODE, citysize, CNTR_CODE, region), as.character))
  
  # Yearly exposure by city: spatial average (only due to no finer population resolution)
  expo_city <- setDT(copy(expodata))
  expo_city <- expo_city %>% 
    group_by(URAU_CODE, date) %>% 
    summarise(
      pm25_spatial = mean(pm25, na.rm = T),
      citysize = first(citysize),
      CNTR_CODE = first(CNTR_CODE),
      region = first(region),
      pop = sum(pop),
      .groups = "drop")
  # calculating the indicator variable for days above selected percentiles
  expo_city <- expo_city %>% 
    mutate(perc50 = ifelse(pm25_spatial>perc["perc50"],1,0),
           perc75 = ifelse(pm25_spatial>perc["perc75"],1,0),
           perc90 = ifelse(pm25_spatial>perc["perc90"],1,0),
           perc95 = ifelse(pm25_spatial>perc["perc95"],1,0),
           perc99 = ifelse(pm25_spatial>perc["perc99"],1,0),
           perc99.9 = ifelse(pm25_spatial>perc["perc99.9"],1,0))
  # store the indicator day variables for later
  expo_city_perc <- expo_city
  # aggregate data by city (and year)
  expo_city <- expo_city %>%
    group_by(URAU_CODE) %>%
    summarise(
      pm25_spatial = mean(pm25_spatial, na.rm = T),
      across(starts_with("perc"), ~sum(., na.rm = T)),
      citysize = first(citysize),
      CNTR_CODE = first(CNTR_CODE),
      region = first(region),
      pop = first(pop),
      .groups = "drop")
  # calculate the population-weights for higher aggregations
  expo_city <- expo_city %>% 
    group_by(CNTR_CODE) %>% 
    mutate(popw_URAU_CNTR = (pop/sum(pop))/length(expo_city %>% pull(CNTR_CODE) %>% unique())) %>% 
    ungroup()
  expo_city <- expo_city %>% 
    group_by(region) %>% 
    mutate(popw_URAU_region = (pop/sum(pop))/length(expo_city %>% pull(region) %>% unique())) %>% 
    ungroup()
  expo_city$year <- yearint
  setDF(expo_city)
  
  # Yearly exposure by city size, population-weighted + spatial average
  expo_citysize <- setDT(copy(expodata))
  # population-weighted exposure
  expo_citysize[, popw := pop/sum(pop), by = .(citysize, date)]
  expo_citysize <- expo_citysize %>% 
    group_by(citysize, date) %>% 
    summarise(pm25_pop = sum(popw*pm25), 
           pm25_spatial = mean(pm25),
           .groups = "drop")
  # merge indicator day variables
  expo_citysize_perc <- expo_city_perc %>%
    group_by(citysize, date) %>%
    summarise(across(starts_with("perc"), ~sum(.,na.rm=T)),
                     .groups = "drop")
  expo_citysize <- expo_citysize %>% 
    full_join(expo_citysize_perc, by = c("citysize", "date"))
  # aggregate data by city size
  expo_citysize <- expo_citysize %>%
    group_by(citysize) %>%
    summarise(
      pm25_pop = mean(pm25_pop, na.rm = T),
      pm25_spatial = mean(pm25_spatial, na.rm = T),
      across(starts_with("perc"), ~sum(.,na.rm=T)),
      .groups = "drop")
  expo_citysize$year <- yearint
  expo_citysize <- expo_citysize %>% 
    select(10,1:9)
  setDF(expo_citysize)
  
  # Yearly exposure by country, population-weighted + spatial average
  expo_country <- setDT(copy(expodata))
  # population-weighted exposure
  expo_country[, popw := pop/sum(pop), by = .(CNTR_CODE, date)]
  expo_country <- expo_country %>% 
    group_by(CNTR_CODE, date) %>% 
    summarise(pm25_pop = sum(popw*pm25), 
              pm25_spatial = mean(pm25),
              .groups = "drop")
  # merge indicator day variables
  expo_country_perc <- expo_city_perc %>%
    group_by(CNTR_CODE, date) %>%
    summarise(across(starts_with("perc"), ~sum(.,na.rm=T)),
              .groups = "drop")
  expo_country <- expo_country %>% 
    full_join(expo_country_perc, by = c("CNTR_CODE", "date"))
  # aggregate data by country
  expo_country <- expo_country %>%
    group_by(CNTR_CODE) %>%
    summarise(
      pm25_pop = mean(pm25_pop, na.rm = T),
      pm25_spatial = mean(pm25_spatial, na.rm = T),
      across(starts_with("perc"), ~sum(.,na.rm=T)),
      .groups = "drop")
  expo_country$year <- yearint
  expo_country <- expo_country %>% 
    select(10,1:9)
  setDF(expo_country)
  
  # Yearly exposure by European region, population-weighted + spatial average
  expo_region <- setDT(copy(expodata))
  # population-weighted exposure
  expo_region[, popw := pop/sum(pop), by = .(region, date)]
  expo_region <- expo_region %>% 
    group_by(region, date) %>% 
    summarise(pm25_pop = sum(popw*pm25), 
              pm25_spatial = mean(pm25),
              .groups = "drop")
  # merge indicator day variables
  expo_region_perc <- expo_city_perc %>%
    group_by(region, date) %>%
    summarise(across(starts_with("perc"), ~sum(.,na.rm=T)),
              .groups = "drop")
  expo_region <- expo_region %>% 
    full_join(expo_region_perc, by = c("region", "date"))
  # aggregate data by country
  expo_region <- expo_region %>%
    group_by(region) %>%
    summarise(
      pm25_pop = mean(pm25_pop, na.rm = T),
      pm25_spatial = mean(pm25_spatial, na.rm = T),
      across(starts_with("perc"), ~sum(.,na.rm=T)),
      .groups = "drop")
  expo_region$year <- yearint
  expo_region <- expo_region %>% 
    select(10,1:9)
  setDF(expo_region)
  
  # Yearly exposure European-wide, population-weighted + spatial average
  expo_euro <- setDT(copy(expodata))
  # population-weighted exposure
  expo_euro[, popw := pop/sum(pop), by = .(date)]
  expo_euro <- expo_euro %>% 
    group_by(date) %>% 
    summarise(pm25_pop = sum(popw*pm25), 
              pm25_spatial = mean(pm25),
              .groups = "drop")
  # merge indicator day variables
  expo_euro_perc <- expo_city_perc %>%
    group_by(date) %>%
    summarise(across(starts_with("perc"), ~sum(.,na.rm=T)),
              .groups = "drop")
  expo_euro <- expo_euro %>% 
    full_join(expo_euro_perc, by = c("date"))
  # aggregate data by country
  expo_euro <- expo_euro %>%
    summarise(
      pm25_pop = mean(pm25_pop, na.rm = T),
      pm25_spatial = mean(pm25_spatial, na.rm = T),
      across(starts_with("perc"), ~sum(.,na.rm=T)))
  expo_euro$year <- yearint
  expo_euro <- expo_euro %>% 
    select(9,1:8)
  setDF(expo_euro)
  
  expo_city <- expo_city %>% 
    select(15,11,10,1:8)
  setDF(expo_city)
  
  setDF(expo_city_perc)
    
  
  # Return data
  list("expo_city" = expo_city,
       "expo_citysize" = expo_citysize,
       "expo_country" = expo_country,
       "expo_region" = expo_region,
       "expo_euro" = expo_euro,
       "expo_city_perc" = expo_city_perc) # used for percentile identification
}



# 2. Execute  ----

expo03 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2003.csv"), 2003)
expo04 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2004.csv"), 2004)
expo05 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2005.csv"), 2005)
expo06 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2006.csv"), 2006)
expo07 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2007.csv"), 2007)
expo08 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2008.csv"), 2008)
expo09 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2009.csv"), 2009)
expo10 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2010.csv"), 2010)
expo11 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2011.csv"), 2011)
expo12 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2012.csv"), 2012)
expo13 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2013.csv"), 2013)
expo14 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2014.csv"), 2014)
expo15 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2015.csv"), 2015)
expo16 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2016.csv"), 2016)
expo17 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2017.csv"), 2017)
expo18 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2018.csv"), 2018)
expo19 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2019.csv"), 2019)
expo20 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2020.csv"), 2020)
expo21 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2021.csv"), 2021)
expo22 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2022.csv"), 2022)
expo23 <- expo_aggr(paste0(pathroot, "data/processed/assembled/data_2023.csv"), 2023)


# Yearly exposure by city, spatial average
expo_city <- bind_rows(expo03[[1]], expo04[[1]], expo05[[1]], expo06[[1]], expo07[[1]],
                        expo08[[1]], expo09[[1]], expo10[[1]], expo11[[1]], expo12[[1]],
                        expo13[[1]], expo14[[1]], expo15[[1]], expo16[[1]], expo17[[1]],
                        expo18[[1]], expo19[[1]], expo20[[1]], expo21[[1]], expo22[[1]],
                        expo23[[1]])
write_csv(expo_city, paste0(pathroot, "data/processed/pm25_city.csv"))

# Yearly exposure by city size, population and spatial average
expo_citysize <- bind_rows(expo03[[2]], expo04[[2]], expo05[[2]], expo06[[2]], expo07[[2]],
                          expo08[[2]], expo09[[2]], expo10[[2]], expo11[[2]], expo12[[2]],
                          expo13[[2]], expo14[[2]], expo15[[2]], expo16[[2]], expo17[[2]],
                          expo18[[2]], expo19[[2]], expo20[[2]], expo21[[2]], expo22[[2]],
                          expo23[[2]])
write_csv(expo_citysize, paste0(pathroot, "data/processed/pm25_citysize.csv"))

# Yearly exposure by country, population and spatial average
expo_country <- bind_rows(expo03[[3]], expo04[[3]], expo05[[3]], expo06[[3]], expo07[[3]],
                         expo08[[3]], expo09[[3]], expo10[[3]], expo11[[3]], expo12[[3]],
                         expo13[[3]], expo14[[3]], expo15[[3]], expo16[[3]], expo17[[3]],
                         expo18[[3]], expo19[[3]], expo20[[3]], expo21[[3]], expo22[[3]],
                         expo23[[3]])
write_csv(expo_country, paste0(pathroot, "data/processed/pm25_country.csv"))

# Yearly exposure by European region, population and spatial average
expo_region <- bind_rows(expo03[[4]], expo04[[4]], expo05[[4]], expo06[[4]], expo07[[4]],
                     expo08[[4]], expo09[[4]], expo10[[4]], expo11[[4]], expo12[[4]],
                     expo13[[4]], expo14[[4]], expo15[[4]], expo16[[4]], expo17[[4]],
                     expo18[[4]], expo19[[4]], expo20[[4]], expo21[[4]], expo22[[4]],
                     expo23[[4]])
write_csv(expo_region, paste0(pathroot, "data/processed/pm25_region.csv"))

# Yearly exposure Europe-wide, population and spatial average
expo_euro <- bind_rows(expo03[[5]], expo04[[5]], expo05[[5]], expo06[[5]], expo07[[5]],
                         expo08[[5]], expo09[[5]], expo10[[5]], expo11[[5]], expo12[[5]],
                         expo13[[5]], expo14[[5]], expo15[[5]], expo16[[5]], expo17[[5]],
                         expo18[[5]], expo19[[5]], expo20[[5]], expo21[[5]], expo22[[5]],
                         expo23[[5]])
write_csv(expo_euro, paste0(pathroot, "data/processed/pm25_euro.csv"))

# Daily exposure city-level, spatial average
pm25_city_daily <- bind_rows(expo03[[6]], expo04[[6]], expo05[[6]], expo06[[6]], expo07[[6]],
                       expo08[[6]], expo09[[6]], expo10[[6]], expo11[[6]], expo12[[6]],
                       expo13[[6]], expo14[[6]], expo15[[6]], expo16[[6]], expo17[[6]],
                       expo18[[6]], expo19[[6]], expo20[[6]], expo21[[6]], expo22[[6]],
                       expo23[[6]])
write_csv(pm25_city_daily, paste0(pathroot, "data/processed/pm25_city_daily.csv"))




# # 2. Exposure percentiles: peak days of PM2.5 above percentiles ----
# 
# pm25_daily <- read_csv(paste0(pathroot, "data/processed/pm25_city_daily.csv"))
# 
# # Calculate thresholds
# percentiles <- c(0.5, 0.75, 0.90, 0.95, 0.99, 0.999)
# thresholds_df <- pm25_daily %>%
#   summarise(quantiles = list(quantile(pm25_pop, probs = percentiles, na.rm = T))) %>%
#   unnest_wider(quantiles)
# thresholds_vector <- as.numeric(thresholds_df)
# names(thresholds_vector) <- names(thresholds_df)
# # thresholds c(0.03766824, 0.13392309,0.41056107, 0.80399332, 2.73855073, 10.84405124)


# clean
rm(list = ls())







