#' @title sumVegMMI: summarize Vegetation Multimetric Index
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#' @importFrom purrr reduce
#'
#' @description This function calculates the Vegetation Multimetric Index (VMMI) for each Rapid Assessment Monitoring (RAM) site in Acadia National Park following Miller, K.M., B.R. Mitchell, and B.J. McGill. 2016. Constructing multimetric indices and testing ability of landscape metrics to assess condition of freshwater wetlands in the Northeastern US. Ecological Indicators. 66:143-152. VMMI ratings of good, fair and poor are also included. Function can filter on site, year, and QAQC status. Coefficient of conservatism and Coeffient of wetness come from the Maine Floristic Quality Assessment of 82 - Acadian Plains Hills: https://www.maine.gov/dacf/mnap/features/coc.htm.
#'
#' @param site Character. Filter on site code. Options are "all" (default) or a vector of site codes ranging from "R-01" to "R-40".
#' @param panel Numeric. Filter on panel number. By default, all panels are returned, and can be filtered by numbers 1 to 4.
#' @param years Numeric. Filter on sample year, ranging from 2012 to 2024. By default, all years are returned. Note that years 2011, 2016, 2021, and 5-year intervals of years thereafter are EPA NWCA sites that are not
#' @param QAQC Logical. Include QAQC visits (TRUE) or drop QAQC visits (FALSE; default).
#'
#' @return Returns a data frame with vegetation MMIs for each site.
#'
#' @examples
#' \dontrun{
#' # import RAM data with protected records- Note that to calculate VMMI correctly,
#' # protected species records must be included
#' importRAM(export_protected = T)
#'
#' # Calculate veg MMI for all sites and all years
#' vegmmi <- sumVegMMI()
#'
#' # Calculate veg MMI for 2024 only
#' vegmmi24 <- sumVegMMI(years = 2024)
#'
#' }
#' @export

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

species_by_strata <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_by_strata_2024.csv") %>%
  as_tibble()

species_list <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_list_2024.csv") %>%
  as_tibble()

locations <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/locations_2024.csv") %>%
  as_tibble()

visits <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/visits_2024.csv") %>%
  as_tibble()


#--------------------------#
####    Run Function    ####
#--------------------------#

sumVegMMI <- function(site = "all", panel = -1, years = 2013:2024,
                      QAQC = FALSE){

  #---- Error Handling ----
  # Make more general for Non-NETN sites
  env <- if(exists("VIEWS_RAM")){VIEWS_RAM} else {.GlobalEnv}
  site_list <- tryCatch(unique(get("locations", envir = env)$Code),
                        error = function(e){stop("The locations table was not found. Please import wetland RAM views.")})

  site <- match.arg(site, c("all", site_list), several.ok = TRUE)
  site <- if(any(site == "all")){site_list} else {site}

  stopifnot(class(panel) %in% c("numeric", "integer"), all(panel %in% c(1, 2, 3, 4, -1)))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2013)
  stopifnot(class(QAQC) == "logical")

  #---- Compile Data ----
  spplist <- tryCatch(get("species_list", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                                              "limited_RAM", "CoC_ME_ACAD", "TSN", "Latin_Name")],
                      error = function(e){stop("The tbl_species_list table was not found. Please import wetland RAM views.")}
                      )
  strata <- get("species_by_strata", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                                         "limited_RAM", "CoC_ME_ACAD", "TSN", "Latin_Name", "Percent_Cover")]

  visit <- get("visits", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type", "limited_RAM",
                                             "Bryophyte_Cover", "Invasive_Cover")]
  loc <- get("locations", envir = env)[,c("Code", "Location_ID", "Panel", "xCoordinate", "yCoordinate", "UTM_Zone")]

  covtol <- strata |>
    mutate(cov_tol = ifelse(CoC_ME_ACAD <= 4, Percent_Cover, 0)) |>
    group_by(Code, Location_ID, Visit_ID, Panel, Year, Visit_Type) |>
    summarize(sum_cov_tol = sum(cov_tol, na.rm = T),
              .groups = "drop")

  meanC <- spplist |> group_by(Code, Location_ID, Visit_ID, Panel, Year, Visit_Type) |>
    summarize(meanC = mean(CoC_ME_ACAD, na.rm = T),
              .groups = "drop")

  comb <- purrr::reduce(list(visit, meanC, covtol), left_join,
                        by = c("Code", "Location_ID", "Visit_ID", "Panel", "Year", "Visit_Type"))

  vmmi_calc <- comb |>
    mutate(meanC_adj1 = ifelse(meanC < 3.015, 3.015, ifelse(meanC > 7.346, 7.346, meanC)),
           meanC_adj2 = ((meanC_adj1 - 3.015)/(7.346 - 3.015)) * 10,

           covtol_adj1 = ifelse(sum_cov_tol < 0.386, 0, ifelse(sum_cov_tol > 136.645, 136.645, sum_cov_tol)),
           covtol_adj2 = ((((covtol_adj1 - 0.386)/(136.645 - 0.386))*10) - 10) * -1,

           invcov_adj1 = ifelse(Invasive_Cover > 38.45, 38.45, Invasive_Cover),
           invcov_adj2 = ((((invcov_adj1/38.45) * 10) - 10))*-1,

           bryo_adj1 = ifelse(Bryophyte_Cover > 98.48, 98.48, Bryophyte_Cover),
           bryo_adj2 = (bryo_adj1/98.48) * 10,

           vmmi = (((meanC_adj2 + covtol_adj2 + invcov_adj2 + bryo_adj2) - 0.389)/(40 - 0.389)) * 100,
           vmmi_rating = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair"))
           )

  vmmi_site <- filter(vmmi_calc, Code %in% site)
  vmmi_year <- filter(vmmi_site, Year %in% years)
  vmmi_panel <- filter(vmmi_year, Panel %in% panel)
  vmmi_qaqc <- if(QAQC == FALSE){filter(vmmi_panel, Visit_Type == "VS")} else {vmmi_panel}

  if(nrow(vmmi_qaqc) == 0){
    stop("Arguments returned a data frame with no records. Be sure you specified RAM years, and not EPA NWCA years.")}

  vmmi_final <- left_join(vmmi_qaqc, loc, by = c("Code", "Location_ID", "Panel")) |>
    select(Code, Location_ID, Visit_ID, Panel, xCoordinate, yCoordinate, UTM_Zone, Date, Year, Visit_Type,
           limited_RAM, meanC, Bryophyte_Cover, Invasive_Cover, Cover_Tolerant = sum_cov_tol,
           vmmi, vmmi_rating)

  return(vmmi_final)

  }

result <- sumVegMMI()
print(result)

