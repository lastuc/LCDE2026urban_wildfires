#-----------------------------------------------------------------------------#
#        LCD Europe urban scale Wildfire Smoke indicator 2026: Makefile       #
#-----------------------------------------------------------------------------#

library("terra")
library("tidyterra")
library("sf")
library("lubridate")
library("ISOweek")
library("tidyverse")
library("data.table")
library("readxl")
library("rio")
library("ggplot2")
library("ggpubr")



# 1. Boundaries and city codes ----
source("R/1_boundary.R")

# 2. Mortality data; annual, weekly and daily counts (HPC) ----
source("R/2_mortality.R")

# 3. Merge exposures, population, NUTS codes - daily (HPC) ----
source("R/3_assemble.R")

# 4. Exposure subindicator by area (HPC) ----
source("R/4_exposure.R")

# 5. Attributable mortality subindicator _ daily HIA (HPC) ----
source("R/6_HIA.R")

# 7. FWI fire risk subindicator ----
source("R/7_FWI.R")

# 8. Figures and tables ----
source("R/8_figtab.R")
