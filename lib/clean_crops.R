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
## Function to select and rename desired columns
cln_dat <- function(raw_data){
  cleaned_data <- raw_data %>%
    select(Year, State, County, "Data Item", Value, "CV (%)")%>%
    rename("year" = "Year", "state"="State", "county"="County", 
           "data_item"= "Data Item", "value"="Value", "cv" = "CV (%)")
  return(cleaned_data)
}

## Function to get state statistics
## Make state regions

states <- unique(midwest$State)
north <- c("IDAHO","MINNESOTA","MONTANA", "NORTH DAKOTA",
           "SOUTH DAKOTA", "WISCONSIN")
south <- c("ARKANSAS","ILLINOIS", "INDIANA", "IOWA", "KANSAS",
           "KENTUCKY", "MISSOURI", "NEBRASKA", "OKLAHOMA",
           "TENNESSEE", "TEXAS")

state_stats <- function(cln_dat){
  # get state stats 
  state_stats <- cln_dat %>%
    group_by(year, state)%>%
    summarize(
      avg = mean(value),
      max = max(value),
      min = min(value)
    )%>%
    mutate(
      region = case_when(
        state %in% north ~ "north",
        state %in% south ~ "south"
      )
    )
  # get national average of each year
  nat_stats <- state_stats%>%
    group_by(year)%>%
    summarize(
      nat_avg = mean(avg) 
    )
  
  # once national trends are known, get % diff from national avg 
  state_stats <- merge(state_stats, nat_stats) %>%
    mutate(
      pct_diff = (avg - nat_avg)/ nat_avg
    )
  
  return(state_stats)
}
## ---------------------------
## IF READING IN DATASET OF ALL STATES  ((PREFERRED))
## Clean Dataset of all midwest states from 1975-2020
midwest1 <- read_csv("./data/NASS_Ar_Kn_1975_2020_CY.csv")  # had to break it up
midwest2 <- read_csv("./data/NASS_Mn_ws_1975_2020_CY.csv")  # to download
midwest <- rbind(midwest1, midwest2)                        # recombine
cln_midwest <- cln_dat(midwest)

midwest_stats <- state_stats(cln_midwest)

head(midwest_stats, 5)
write.csv(midwest_stats, file= "./output/midwest_crop_yields.csv")