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

#-----------------------#
####    Data Manip   ####
#-----------------------#

## 2013-2015 Glen Veg data ## --------------------------------------------------

## combine tlu_Plant to species.list by common columns

# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols <- intersect(setdiff(names(species.list), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))

# Step 3: Join and update values
species.list.tlu <- species.list %>%
  left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1

setdiff(names(species.list), names(species.list.tlu))

# Save outputs as CSV
# write.csv(species.list.tlu, "data/processed_data/species_list_tlu.csv", row.names = FALSE)


## combine tlu_Plant to species.by.strata by common columns

# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols_2 <- intersect(setdiff(names(species.by.strata), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub_2 <- tlu_Plant %>% select(all_of(c(key_col, common_cols_2)))

# Step 3: Join and update values
species.by.strata.tlu <- species.by.strata %>%
  left_join(tlu_Plant_sub_2, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols_2), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1

setdiff(names(species.by.strata), names(species.by.strata.tlu))

# Save outputs as CSV
# write.csv(species.by.strata.tlu, "data/processed_data/species_by_strata_tlu.csv", row.names = FALSE)




