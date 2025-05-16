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

species_by_strata_2015 <- read.csv("data/raw_data/Glen_veg_data/species_by_strata_2015_2023.csv") %>%
  as_tibble()

species_list_2015 <- read.csv("data/raw_data/Glen_veg_data/species_list_2015_2023.csv") %>%
  as_tibble()

tlu_Plant <- read.csv("data/raw_data/Glen_veg_data/tlu_Plant.csv") %>%
  as_tibble()

locations_2015 <- read.csv("data/raw_data/Glen_veg_data/locations_2015_2023.csv") %>%
  as_tibble()

visits_2015 <- read.csv("data/raw_data/Glen_veg_data/visits_2015_2023.csv") %>%
  as_tibble()


#-----------------------#
####    Data Manip   ####
#-----------------------#

## 2013-2015 Glen Veg data ## --------------------------------------------------

## combine tlu_Plant to species.list by common columns

#first find latin names that are in species_list and not in tlu_Plant
latin_mismatch_list <- anti_join(species_list_2015, tlu_Plant, by = "Latin_Name")

#search tlu for mismatch latin names
#search_tlu <- filter(tlu_Plant, Accepted_Latin_Name == "Carex lenticularis")

#rename latin name species using case_when
species_list_new <- species_list_2015 %>% 
  
  mutate(
    Latin_Name =
      case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
                Latin_Name == "Lysimachia borealis" ~ "Trientalis borealis",
                Latin_Name == "Glyceria laxa" ~ "Glyceria canadensis",
                Latin_Name == "Viola pallens" ~ "Viola macloskeyi",
                Latin_Name == "Rhodora" ~ "Rhododendron canadense",
                Latin_Name == "Malus sieboldii" ~ "Malus toringo",
                Latin_Name == "Rubus idaeus spp strigosus" ~ "Rubus idaeus",
                Latin_Name == "Juncas pylaei" ~ "Juncus effusus",
                Latin_Name == "Dulichium" ~ "Dulichium arundinaceum",
                Latin_Name == "Unknown Agrostis" ~ "Agrostis",
                Latin_Name == "Brachyelytrum" ~ "Brachyelytrum aristosum",
                Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
                Latin_Name == "Potamogeton berchtoldii" ~ "Potamogeton pusillus",
                Latin_Name == "Brachyelytum aristosum" ~ "Brachyelytrum aristosum",
                Latin_Name == "Juncus pylaei" ~ "Juncus effusus",
                Latin_Name == "Utricularia vulgaris" ~ "Utricularua vulgaris",
                TRUE ~ Latin_Name))

#check renaming worked
check_latin_mismatch_list <- anti_join(species_list_new, tlu_Plant, by = "Latin_Name")

#combine datasets
# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols <- intersect(setdiff(names(species_list_new), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))

# Step 3: Join and update values
species_list_tlu <- species_list_new %>%
  left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1
setdiff(names(species_list_new), names(species_list_tlu))

#fix latin name again
species_list_tlu <- species_list_tlu %>% 
mutate(
  Latin_Name =
    case_when(Latin_Name == "Utricularua vulgaris" ~ "Utricularia vulgaris",
              TRUE ~ Latin_Name))          

# Save outputs as CSV
# write.csv(species_list_tlu, "data/processed_data/species_list_tlu_2015_2023.csv", row.names = FALSE)



## combine tlu_Plant to species_by_strata by common columns---------------------

#first find latin names that are in species_by_strata and not in tlu_Plant
strata_latin_mismatch <- anti_join(species_by_strata_2015, tlu_Plant, by = "Latin_Name")

#rename latin name species using case_when
species_by_strata_new <- species_by_strata_2015 %>% 
  
  mutate(
    Latin_Name =
      case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
                Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
                TRUE ~ Latin_Name))

#combine datasets
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
# write.csv(species_by_strata_tlu, "data/processed_data/species_by_strata_tlu_2015_2023.csv", row.names = FALSE)






