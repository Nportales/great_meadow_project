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

#-----------------------#
####    Data Manip   #### 
#-----------------------#

# Remove one column
VMMI_2024 <- VMMI_2024 %>% select(-X)

# Add site column to 2015-2023 data
VMMI_2015_2023 <- VMMI_2015_2023 %>% 
  mutate(Site = "Great Meadow")

# merge 2015-2023 data and 2024 data
VMMI_2015_2024 <- bind_rows(VMMI_2015_2023, VMMI_2024)


