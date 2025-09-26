#-----------------------------------------------------------------------------#
#          2. Clean and downscale annual and weekly mortality data            #
#-----------------------------------------------------------------------------#


# Warnings:

# Weekly mortality from EUROSTAT:
# Germany (DE), Ireland (IE), Croatia (HR), Slovenia (SI), Estonia (EE) available at the NUTS1 level
# No data for Macedonia (MK)
# The rest of the countries are available at NUTS 3 level

# annual city-level mortality prepared for LCDE urban scale 2026
# available 2000-2023
# 885 cities included


# pathroot <- "/PROJECTES/AIRPOLLUTION/lara/LCDE2026_wildfires/"
pathroot <- ""


# 1. Clean weekly mortality ----

# Read data
mort <- fread(paste0(pathroot, "data/raw/mortality/estat_demo_r_mwk3_ts.tsv"))
# Take years 2003-2024
mort <- mort %>% select(1, contains(as.character(2003:2023)))
# Disentangle 1st column, take both sexes and NUTS codes
names(mort)[1] <- "meta"
mort <- group_by(mort, meta) %>% 
  mutate(sex = strsplit(meta, ",")[[1]][2],
         NUTS_mort = strsplit(meta, ",")[[1]][4]) %>% 
  ungroup() %>% 
  filter(sex == "T") %>%  # I may run a sex specific HIA as well
  # nuts3 # = 1215; no nuts3: DE, IE, HR, MT (no exp for islands), SI -> only available in NUTS1;
  filter((nchar(NUTS_mort) < 5 & grepl("^DE|^IE|^HR|^SI", NUTS_mort)) | nchar(NUTS_mort) == 5) %>% 
# use the proportions from the annual mortality data set of total deaths in nuts1 or in the country annually and apply these proportions to the weekly mortality -> check with José
  dplyr::select(-meta, -sex) 
# Wide to long format, parse NAs and remove provisional flags
mort <- pivot_longer(mort, -NUTS_mort, names_to = "week", values_to = "deaths")
mort <- mutate(mort,
                country = substr(NUTS_mort,1,2),
                deaths = ifelse(deaths == ":", NA, deaths),
                deaths = gsub(" p", "", deaths),
                deaths = as.numeric(deaths)) %>%
  filter(!is.na(deaths))
# Discard XXX in NUTS3: unclassified and minor counts, no geometry available
mort <- mort %>% filter(!grepl("XXX", NUTS_mort))

# Cleaning
# Discard more NUTS3 for which we have no exposure data: Canary Islands (ES70),
# Guadeloupe (FRY1), Martinique (FRY2), Guyane (FRY3), La Reunion (FRY4),
# Mayotte (FRY5), Acores (PT20), Madeira (PT30)
mort <- mort[!grepl("ES70|FRY1|FRY2|FRY3|FRY4|FRY5|PT20|PT30", mort$NUTS_mort),] 

# Merging split areas after 2021 to match regions from the old NUTS definitions 2021 or older
mort <- mort %>% filter(!NUTS_mort %in% c("PT1A0", "PT1B0")) %>%
  filter(!NUTS_mort %in% c("NO072", "NO073")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NO072", "NO073")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NO074", 
                deaths = sum(deaths))) %>%
  filter(!NUTS_mort %in% c("NO083", "NO084", "NO085")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NO083", "NO084", "NO085")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NO082", 
                deaths = sum(deaths))) %>%
  filter(!NUTS_mort %in% c("NO093", "NO094")) %>%
  bind_rows(mort %>%
              filter(NUTS_mort %in% c("NO093", "NO094")) %>% 
              group_by(week) %>% 
              summarise(
                NUTS_mort = "NO091", 
                deaths = sum(deaths)))
mort <- arrange(mort, NUTS_mort, week)

# Reducing the NUTS3/NUTS1 to those containing a city
city00_14 <- vect(paste0(pathroot, "data/raw/boundaries/cities_2000-14.shp")) %>% as_tibble()
city15_24 <- vect(paste0(pathroot, "data/raw/boundaries/cities_lcde.shp")) %>% as_tibble()
city00_24 <- city00_14 %>% full_join(city15_24) %>% select(URAU_CODE, URAU_NAME, CNTR_CODE, FUA_CODE, NUTS3_2021) %>%
  distinct() %>%
  mutate(NUTS1_2021 = str_sub(NUTS3_2021, start = 1, end = 3))

mort <- mort %>% inner_join(city00_24, by = c("NUTS_mort" = "NUTS3_2021")) %>% # need to check!
  bind_rows(mort %>% inner_join(city00_24, by = c("NUTS_mort" = "NUTS1_2021"))) %>%
  mutate(NUTS1_2021 = ifelse(is.na(NUTS1_2021), NUTS_mort, NUTS1_2021),
         NUTS3_2021 = ifelse(is.na(NUTS3_2021), NUTS_mort, NUTS3_2021))
rm(city00_14, city15_24, city00_24)



# 2. Read, split up to weekly mortality ----

# Import annual city-level mortality 
mort_yr <- read_csv("data/raw/mortality/LCDE-prep/cities_deaths_lcde_vfinal.csv") %>% 
  select(1:7)

# Create a data frame that includes all weeks for each year and URAU_CODE
mort_yr_wk <- mort_yr %>%
  group_by(URAU_CODE, Year) %>%
  mutate(
    start_date = ymd(paste0(Year, "-01-01")),
    end_date = ymd(paste0(Year, "-12-31")),
    # Create a sequence of all dates within the year
    all_dates = list(seq(start_date, end_date, by = "day"))) %>%
  unnest(all_dates) %>%
  # Add the 'week' variable in the desired format
  mutate(week = paste0(year(all_dates), "-W", sprintf("%02d", isoweek(all_dates)))) %>%
  # Keep only the first entry for each week
  distinct(URAU_CODE, Year, week, .keep_all = T) %>%
  select(-start_date, -end_date, -all_dates) %>%
  ungroup()



# 3. and merge with EUROSTA mortality ----

# Join with the full weekly data frame on both 'URAU_CODE' and 'year'
mort_wk <- mort_yr_wk %>% 
  full_join(mort, by = c("URAU_CODE" = "URAU_CODE", "week" = "week"))
  # Get the number of weeks in each year, grouped by URAU_CODE
mort_wk <- mort_wk %>% 
  filter(!is.na(deaths_total)) %>% 
  group_by(URAU_CODE, Year) %>% 
  mutate(total_wk = n()) %>% 
  ungroup() %>% 
  mutate(deaths_total_wk = deaths_total / total_wk) %>% 
  select(1:10,13,17,18)



# 4. Apply weekly trends of NUTS3 mortality to city-level mortality using ratios ----

mort_wk <- mort_wk %>% 
  group_by(Year, nuts3) %>% 
  mutate(deaths_yr_nuts3 = sum(deaths)) %>% 
  ungroup() %>%
  mutate(deaths_wk_ratio = deaths/(deaths_yr_nuts3/total_wk), 
         deaths_wk_urb = deaths_total_wk*deaths_wk_ratio) %>% 
  select(-c(deaths_yr_nuts3, deaths_wk_ratio))

# Copy the constant weekly city-level where no weighted seasonal trends in NUTS3 is available
mort_wk <- mort_wk %>% 
  mutate(deaths_wk_urb_merge = ifelse(is.na(deaths_wk_urb), deaths_total_wk, deaths_wk_urb))
   


# 5. Add full variable names and clean ----

# Adding city size and full name variables (based on UN geoscheme)
citysize <- import(paste0(pathroot, "data/raw/cities/List_cities.xlsx")) %>%
  dplyr::select(1,3,4,6,7) %>% distinct()
names(citysize) <- c("CNTR_NAME", "URAU_CODE", "URAU_NAME", "region", "citysize")
# merge XXL and Global city city size categories from UN geoscheme into one category (≥XXL)
citysize <- citysize %>%
  mutate(citysize = ifelse(citysize %in% c("XXL", "Global city"), "≥XXL", citysize))
# merge
mort_wk <- mort_wk %>% select(-URAU_NAME.x, -country.x) %>% rename(year=Year) %>% 
  left_join(citysize, by = "URAU_CODE")

# DROP cities with no exposure (cities on ISLANDS in ES and PT): 
# ES008C, ES024C, ES027C, ES055C, ES057C, ES096C, PT007C
mort_wk <- mort_wk %>% 
  filter(!URAU_CODE %in% c("ES008C", "ES024C", "ES027C", "ES055C", "ES057C", "ES096C", "PT007C")) %>% 
  distinct()



# 6. Split weekly to daily mortality counts ----

# Expand weeks to days by dividing all death counts by 7 and stacking 7 times
mort_day <- mort_wk %>% distinct() %>% mutate(deaths_urb_merge_day = deaths_wk_urb_merge/7,
                            deaths_urb_season_day = deaths_wk_urb/7,
                            deaths_total_day = deaths_total_wk/7)
mort_day <- bind_rows(mutate(mort_day, year_week_day = paste0(week, "-1")),
                       mutate(mort_day, year_week_day = paste0(week, "-2")),
                       mutate(mort_day, year_week_day = paste0(week, "-3")),
                       mutate(mort_day, year_week_day = paste0(week, "-4")),
                       mutate(mort_day, year_week_day = paste0(week, "-5")),
                       mutate(mort_day, year_week_day = paste0(week, "-6")),
                       mutate(mort_day, year_week_day = paste0(week, "-7"))) 
mort_day <- mutate(mort_day, date = ISOweek::ISOweek2date(year_week_day))

# clean data set
mort_day <- arrange(mort_day, URAU_CODE, date) %>% 
  select(-c(year, year_week_day, deaths_wk_urb_merge, deaths_wk_urb, deaths_total_wk, deaths)) %>% 
  mutate(year = lubridate::year(date)) 

# Filter by days and write
mort_day <- filter(mort_day, between(date, as.Date("2003-01-01"), as.Date("2023-12-31"))) %>% 
  mutate(year = year(date)) %>% 
  select(-c(nuts3, week, total_wk))

write_csv(mort_day, paste0(pathroot, "data/processed/mortality.csv"))

# clean
rm(list = ls())
