## Merging Great Meadow Veg Data ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

VMMI_2015_2023 <- read.csv("data/processed_data/VMMI_2015_2023.csv") %>%
  as_tibble()

VMMI_2024 <- read.csv("data/processed_data/ACAD_Wetland_VegMMI_20241216.csv") %>%
  as_tibble()

## 2015-2024 Glen Veg data ##

species_by_strata <- read.csv("data/raw_data/Glen_veg_data/species_by_strata_2015_2024.csv") %>%
  as_tibble()

species_list <- read.csv("data/raw_data/Glen_veg_data/species_list_2015_2024.csv") %>%
  as_tibble()

locations <- read.csv("data/raw_data/Glen_veg_data/locations_2015_2024.csv") %>%
  as_tibble()

visits <- read.csv("data/raw_data/Glen_veg_data/visits_2015_2024.csv") %>%
  as_tibble()

RAM_stressors <- read.csv("data/raw_data/Glen_veg_data/RAM_stressors_2015_2024.csv") %>%
  as_tibble()

vertical_complexity <- read.csv("data/raw_data/Glen_veg_data/vertical_complexity_2015_2024.csv") %>%
  as_tibble()

## Kate Veg data ##

ram_ssplist <- read.csv("data/processed_data/Kate_NETN_veg_data/ram_spplist_2012_to_2022.csv") %>%
  as_tibble()

ram_veg_cover <- read.csv("data/processed_data/Kate_NETN_veg_data/ram_veg_cover_2012_to_2022.csv") %>%
  as_tibble()

sen_ram_species <- read.csv("data/processed_data/Kate_NETN_veg_data/sen_ram_species_data_2011_2022.csv") %>%
  as_tibble()

sen_veg_cover <- read.csv("data/processed_data/Kate_NETN_veg_data/sen_veg_cover_2011_2016_2021.csv") %>%
  as_tibble()

VMMI_ram_sen <- read.csv("data/processed_data/Kate_NETN_veg_data/vegMMI_2011_to_2023.csv") %>%
  as_tibble()


#-----------------------#
####    Data Manip   #### 
#-----------------------#

## merge 2025-2023 and 2024 Glen veg data --------------------------------------

# Remove one column
VMMI_2024 <- VMMI_2024 %>% select(-X)

# Add site column to 2015-2023 data
VMMI_2015_2023 <- VMMI_2015_2023 %>% 
  mutate(Site = "Great Meadow")

# merge 2015-2023 data and 2024 data
VMMI_2015_2024 <- bind_rows(VMMI_2015_2023, VMMI_2024)


## merge Glen VMMI data with NETN VMMI data ------------------------------------

# first edit Glen VMMI data columns to match NETN VMMI data columns
new_VMMI_2015_2024 <- VMMI_2015_2024 %>% 
  
  mutate(
    
    # add a site type column
    site.type = 
      case_when(
        grepl("GRME", Code) ~ "GRME",
        grepl("GIME", Code) ~ "GILM",
        TRUE ~ NA_character_)) %>% 

    # Remove unneeded columns and rename
    select(site.name = Code,
           local.id = Location_ID,
           site.type,
           year = Year,
           xcoord = xCoordinate,
           ycoord = yCoordinate,
           mean.coc = meanC,
           inv.cov = Invasive_Cover,
           bryo.cov = Bryophyte_Cover,
           strtol.cov = Cover_Tolerant,
           vmmi,
           vmmi.rating = vmmi_rating,
           notes = Site)

# then select appropriate sites from NETN VMMI dataset
new_VMMI_ram_sen <- VMMI_ram_sen %>% 
  filter(local.id %in% c("GRME", "GILM")) 

# then merge datasets
VMMI_Glen_NETN <- bind_rows(new_VMMI_2015_2024, new_VMMI_ram_sen)



