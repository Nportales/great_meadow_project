## Great Meadow and the Gilmore Meadow well summary data graphs
## Schoodic Institute at Acadia National Park

#------------------------------------------------#
####                Packages                  ####
#------------------------------------------------#
library(tidyverse)
library(lubridate)

# devtools::install_github("KateMMiller/wetlandACAD")




#------------------------------------------------#
####                Functions                 ####
#------------------------------------------------#

## Reading in Kate Millers functions manually because they weren't coming through
calc_WL_stats <- function(df, from = 2013, to = 2019) {
  
  EDT<-"America/New_York"
  well_prp <- df %>% mutate(timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M"),
                            month = lubridate::month(timestamp),
                            mon = months(timestamp, abbreviate = T)) %>%
    filter(doy > 134 & doy < 275) %>% droplevels()
  
  well_prp2 <- well_prp %>% group_by(Year) %>%
    mutate(lag.precip = lag(precip_cm, 1)) %>%
    ungroup()
  
  well_prp_yr <- well_prp2 %>% filter(between(Year, from, to)) %>% droplevels()
  
  # May 1 DOY= 121; May 15 = 135; Oct.1 = 274
  well_prp_long <- well_prp_yr %>% gather("site","water_level_cm",
                                          -timestamp, -Date, -doy, -Year, -hr,
                                          -doy_h, -month, -mon, -precip_cm, -lag.precip)
  
  well_prp_long2 <- well_prp_long %>% group_by(Year, site) %>%
    mutate(lag_WL = lag(water_level_cm),
           change_WL = water_level_cm-lag_WL)
  
  # Calculate growing season stats
  well_gs_stats <- well_prp_long2 %>% group_by(Year, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
              WL_sd = sd(water_level_cm, na.rm = TRUE),
              WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
              WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
              max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
              max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
              prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100)
  
  # Calculate change in WL from average Jun to average September
  well_gs_month <- well_prp_long2 %>% group_by(Year, mon, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE)) %>%
    filter(mon %in% c("Jun","Sep")) %>% droplevels() %>% spread(mon, WL_mean) %>%
    mutate(GS_change = Sep - Jun)
  
  
  well_gs_prop1 <- well_prp_long2 %>% mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                             bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                                    !is.na(water_level_cm), 1, 0),
                                             under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                             num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))
  
  well_gs_prop <- well_gs_prop1 %>% group_by(Year, site) %>%
    summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100)
  
  gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) %>%
    reduce(left_join, by = c("Year", "site"))
  
  # Missing water level data from 2017, change to NA
  metrics<-c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )
  
  gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics]<-NA
  # Logger failed in DUCK in 2017
  
  prop_complete_check <- length(gs_WL_stats$prop_GS_comp[gs_WL_stats$prop_GS_comp < 90])
  
  if(prop_complete_check > 0) {
    message(paste0("Warning: There are ", prop_complete_check, " sites that have water level measurements for less than 90% of growing season."))
  }
  
  return(gs_WL_stats)
}





plot_hydro_site_year <- function(df, yvar, site, years = 2013:as.numeric(format(Sys.Date(), "%Y"))) {
  
  dat <- df %>% filter(Year %in% years) %>%
    filter(doy > 134 & doy < 275) %>%
    select(doy_h, yvar, Year, lag.precip) %>% droplevels()
  
  colnames(dat)<-c('doy_h', 'WL', 'Year', 'lag.precip')
  
  minWL <- min(dat$WL, na.rm = TRUE)
  
  p <- ggplot(dat, aes(x = doy_h, y = WL, group = Year)) +
    geom_line(col = 'black') +
    geom_line(aes(x = doy_h, y = lag.precip*5 + minWL, group = Year), col ='blue') +
    facet_wrap(~Year, nrow = length(unique(dat$Year))) +
    geom_hline(yintercept = 0, col = 'brown') +
    # ylim(min(df$WL) + 10, max(df$WL) + 10) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11)) +
    labs(title = site, y = 'Water Level (cm)\n', x = 'Date') +
    scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274),
                       labels = c('May-01', 'Jun-01',
                                  'Jul-01', 'Aug-01',
                                  'Sep-01', 'Oct-01')) +
    scale_y_continuous(sec.axis = sec_axis(~.,
                                           breaks = c(minWL, minWL + 10),
                                           name = 'Hourly Precip. (cm)\n',
                                           labels = c('0', '2')))
  return(p)
}


#------------------------------------------------#
####             Read and format              ####
#------------------------------------------------#

### Water Level Calculations
## Read in the data and format for running through Kate's function
gmwell <- tibble(read.csv("data/processed_data/great_meadow_well_data_2024_20250520.csv")) %>% 
  rename(Date = date, Year = year, precip_cm = precip.cm) %>% 
  select(timestamp, Date, doy, Year, precip_cm, water.depth, lag.precip, hr, doy_h, plot.num) %>% 
  mutate(timestamp = as_datetime(timestamp),
         water.depth = ifelse(Year == 2016 & doy_h == 159.12 & plot.num == 3 & 
                                water.depth < -120, NA, water.depth),
         water.depth = ifelse(Year == 2017 & doy_h == 215.02 & plot.num == 6 & 
                                water.depth < -115, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 224 &
                                water.depth > 400, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 225 &
                                water.depth > 400, NA, water.depth))


## Create function to loop that calculates water level stats for each of the six plots
wl_stats_loop <- function(plot.number) {
  
  output <- gmwell %>% 
    filter(plot.num == plot.number) %>% 
    select(-plot.num) %>% 
    calc_WL_stats(., from = 2016, to = 2024) %>% 
    mutate(site = paste0("great.meadow.", plot.number))
  
  return(output)
}


## Calculate the water level stats; 2015 does not have enough data so skipping here
gm_wl_stats <- map_dfr(1:6, ~wl_stats_loop(.))


## Gilmore Meadow
gilm <- tibble(read.csv("data/gilmore_well_prec_data_2013-2022.csv")) %>% 
  rename(gilmore.meadow = GILM_WL)

gil_wl_stats <- calc_WL_stats(gilm, from = 2016, to = 2022)


## Merge the two meadow wl stats
wl_stats <- bind_rows(gm_wl_stats, gil_wl_stats) %>% 
  select(site, Year, everything())

## Pivot into a table for exploration
wl_table <- wl_stats %>% 
  pivot_longer(cols = WL_mean:prop_under_neg30cm) %>% 
  pivot_wider(names_from = site, values_from = value) %>% 
  select(year = Year, stat = name, gilmore.meadow, everything())


## Write out the water level stats
write_csv(wl_table, "outputs/gm_gl_wl_stats.csv")





### Create hydrology graphs
## Format data
## Great Meadow
gm <- gmwell %>%
  mutate(water.depth = ifelse(Year == 2016 & doy_h == 159.12 & plot.num == 3 & 
                              water.depth < -120, NA, water.depth),
         water.depth = ifelse(Year == 2017 & doy_h == 215.02 & plot.num == 6 & 
                                water.depth < -115, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 224 &
                                water.depth > 400, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 225 &
                                water.depth > 400, NA, water.depth)) %>% 
  mutate(site = paste("Great Meadow", plot.num))

## Gilmore Meadow
gl <- gilm %>% 
  mutate(site = paste("Gilmore Meadow"))

## Create a list of the sites to plot
site.list <- gm %>% 
  select(site) %>% 
  distinct() %>% 
  mutate(freq = 7) %>% 
  uncount(freq) %>% 
  mutate(year = rep(2016:2022, 6),
         string = paste(site, year, sep = "_")) %>% 
  select(string) %>% 
  unlist()


## Write hydrograph creating function
# hydrograph <- function (siteyear) {
#   
#   sitename <- str_extract(siteyear, "([^_]*)")
#   
#   year <- str_extract(siteyear, "[^_]*$")
#   
#   dat <- gm %>% filter(site == sitename)
# 
#   plot_hydro_site_year(df = dat, yvar = "water.depth", site = sitename, years = year)
#   
#   
#   
#   ggsave(paste0("outputs/hydrographs/", str_replace_all(sitename, "\\s", "_"),
#                 "_", year, ".png"), width = 8, height = 6)
#     
# }


hydrograph <- function (siteyear) {
  
  sitename <- str_extract(siteyear, "([^_]*)")
  
  year <- str_extract(siteyear, "[^_]*$")
  
  dat <- gm %>% filter(site == sitename)
  
  dat2 <- dat %>% filter(Year == year) %>%
    filter(doy > 134 & doy < 275) %>%
    select(doy_h, water.depth, Year, lag.precip) %>% droplevels()
  
  colnames(dat2) <- c('doy_h', 'WL', 'Year', 'lag.precip')
  
  gil <- gl %>% filter(Year == year) %>%
    filter(doy > 134 & doy < 275) %>%
    select(doy_h, water.depth, Year, lag.precip) %>% droplevels()
  
  colnames(gil) <- c('doy_h', 'WL', 'Year', 'lag.precip')
  
  minWL1 <- min(dat2$WL, na.rm = TRUE)
  minWL2 <- min(gil$WL, na.rm = TRUE)
  
  minWL <- min(minWL1, minWL2)

  ggplot(dat2, aes(x = doy_h, y = WL, group = Year)) +
    geom_line(aes(color = 'black')) +
    geom_line(aes(x = doy_h, y = lag.precip*5 + minWL, group = Year), color ='blue', size = 0.8) +
    geom_line(data = gil, aes(color = 'darkgray')) +
    facet_wrap(~Year, nrow = length(unique(dat$Year))) +
    geom_hline(yintercept = 0, color = 'brown') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11)) +
    labs(title = sitename, y = 'Water Level (cm)\n', x = 'Date') +
    scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274),
                       labels = c('May-01', 'Jun-01',
                                  'Jul-01', 'Aug-01',
                                  'Sep-01', 'Oct-01')) +
    scale_y_continuous(sec.axis = sec_axis(~.,
                                           breaks = c(minWL, minWL + 10),
                                           name = 'Hourly Precip. (cm)\n',
                                           labels = c('0', '2'))) +
    scale_color_manual(values = c("black" = "black", "darkgray" = "darkgray"),
                       labels = c("Great Meadow WL", "Gilmore Meadow WL"),
                       guide = "legend")


  ggsave(paste0("outputs/hydrographs/", str_replace_all(sitename, "\\s", "_"),
              "_", year, ".png"), width = 8, height = 6.7)
}



## Loop through to make each hydrograph
map(site.list, ~hydrograph(.))


