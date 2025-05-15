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

species_by_strata <- read.csv("data/raw_data/Glen_2015_2023_veg_data/species_by_strata(2015_2023).csv") %>%
  as_tibble()

species_list <- read.csv("data/raw_data/Glen_2015_2023_veg_data/species_list.csv") %>%
  as_tibble()

tlu_Plant <- read.csv("data/raw_data/Glen_2015_2023_veg_data/tlu_Plant.csv") %>%
  as_tibble()

locations <- read.csv("data/raw_data/Glen_2015_2023_veg_data/locations.csv") %>%
  as_tibble()

visits <- read.csv("data/raw_data/Glen_2015_2023_veg_data/visits.csv") %>%
  as_tibble()


#-----------------------#
####    Data Manip   ####
#-----------------------#

## 2013-2015 Glen Veg data ## --------------------------------------------------

## combine tlu_Plant to species.list by common columns

#first find columns with missing information
na_rows_list <- species_list_tlu %>% filter(is.na(TSN))

# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols <- intersect(setdiff(names(species_list), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))

# Step 3: Join and update values
species_list_tlu <- species_list %>%
  left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1

setdiff(names(species_list), names(species_list_tlu))

# Save outputs as CSV
# write.csv(species_list_tlu, "data/processed_data/species_list_tlu.csv", row.names = FALSE)


## combine tlu_Plant to species_by_strata by common columns

#first find columns with missing information
na_rows <- species_by_strata_tlu %>% filter(is.na(TSN))

#rename latin name species using casewhen
species_by_strata_new <- species_by_strata %>% 

mutate(
  Latin_Name =
    case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
              Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
              TRUE ~ Latin_Name))

# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols_2 <- intersect(setdiff(names(species_by_strata_new), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub_2 <- tlu_Plant %>% select(all_of(c(key_col, common_cols_2)))

# Step 3: Join and update values
species_by_strata_tlu <- species_by_strata_new %>%
  left_join(tlu_Plant_sub_2, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols_2), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1
setdiff(names(species_by_strata_new), names(species_by_strata_tlu))




# Save outputs as CSV
# write.csv(species_by_strata_tlu, "data/processed_data/species_by_strata_tlu.csv", row.names = FALSE)






