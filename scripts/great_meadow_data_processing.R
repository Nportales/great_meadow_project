## Great Meadow data processing ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## 2013-2015 Glen Veg data ##

species.by.strata <- read.csv("data/raw_data/species_by_strata(2013_2023).csv") %>%
  as_tibble()

species.list <- read.csv("data/raw_data/species_list.csv") %>%
  as_tibble()

tlu_Plant <- read.csv("data/raw_data/tlu_Plant.csv") %>%
  as_tibble()

## 2013-2015 Glen Veg data ##


