## ---------------------------
##
## Script name: clean_crops
##
## Purpose of script: To clean USDA crop yield data, consolidate data of 
## different states into one file
##
## Author: Natalie Williams
##
## Date Created: 2021-03-30
##
## ---------------------------
##
## Notes: This file has 2 methods for cleaning data, then getting 
## statistics for each state.
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory (mac)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)

## ---------------------------
## Read in crop yield data with format "./data/NASS_XX_2000_2020_CY.csv"
## where XX is the state code. 
file_names <- list.files(path = "./data", pattern='*\\_2000_2020_CY.csv')

# use paste to add "./data/ to each name 
file_paths <- paste("./data/", sep = "", file_names)

# apply read_csv to every item in list
state_csv <- lapply(file_paths, read_csv)

## ---------------------------
## Clean data & put into one Dataframe
state_stats_list <- lapply(lapply(state_csv, cln_dat), state_stats)

# Make one data frame of all states info
state_stats_df <- as.data.frame(do.call(rbind, state_stats_list))

head(state_stats_df)
write.csv(state_stats_df, file= "./output/state_crop_yields.csv")

## ---------------------------
## Make graph
g1 <- ggplot(state_stats_df, aes(x=year, y=state_avg))+
  geom_point(aes(color=state))+
  geom_smooth(aes(color=state))+
  labs(
    title = str_wrap("How have the crop yields of US States changed since 2000?", width=40),
    x= "Year",
    y= "Corn Bushels per Acre"
  )+
  theme_classic()+
  theme(legend.position = 'bottom')

g1
ggsave(path = "./figs", filename = "fig1.png")

## ---------------------------
## Function to select and rename desired columns
cln_dat <- function(raw_data){
  cleaned_data <- raw_data %>%
    select(Year, State, County, "Data Item", Value, "CV (%)")%>%
    rename("year" = "Year", "state"="State", "county"="County", 
           "data_item"= "Data Item", "value"="Value", "cv" = "CV (%)")
  
  return(cleaned_data)
}

## Function to get state statistics
state_stats <- function(cln_dat){
  state_stats <- cln_dat %>%
    group_by(year, state)%>%
    summarize(
      state_avg = mean(value),
      state_max = max(value),
      state_min = min(value),
      best_county = county[which.max(value)],
      worst_county = county[which.min(value)]
    )
  return(state_stats)
}

## ---------------------------
## DEMO 
# Iowa
# FROM URL=https://quickstats.nass.usda.gov/results/6100C076-DF33-3555-A8BA-CF8D964BEB79
ia_crop_yields <- read_csv("./data/NASS_IA_2000_2020_CY.csv")
print(demo, n=10)

# Oklahoma
# FROM URL=https://quickstats.nass.usda.gov/results/92330E53-1F13-35AE-8371-B58EDB3C6D17
ok_crop_yields <- read_csv("./data/NASS_OK_2000_2020_CY.csv")
print(ok_crop_yields, n=5)

# North Dakota
# FROM URL=https://quickstats.nass.usda.gov/results/C5917E5F-9E13-3C7C-A59F-7EAE7C961C62
nd_crop_yields <- read_csv("./data/NASS_ND_2000_2020_CY.csv")

## ---------------------------
# Extract data from loaded csv files

ok_stats <- state_stats(cln_dat(ok_crop_yields))

ia_stats <- state_stats(cln_dat(ia_crop_yields))

nd_stats <- state_stats(cln_dat(nd_crop_yields))

ok_ia_nd <-rbind(ok_stats, ia_stats, nd_stats)


print(ok_ia_nd, n=4)
## ---------------------------
# Make plot
g1 <- ggplot(ok_ia_nd, aes(x=year, y=state_avg))+
  geom_point(aes(color=state))+
  geom_smooth(aes(color=state))+
  labs(
    title = str_wrap("How have the crop yields of Three US States changed since 2000?", width=40),
    x= "Year",
    y= "Corn Bushels per Acre"
  )+
  theme_classic()+
  theme(legend.position = 'bottom')

g1

