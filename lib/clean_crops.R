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
setwd(natalie_wd)     # Natalie's working directory (pc)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)
library(priceR) # for price adjustment


## ---------------------------
## Function to select and rename desired columns
cln_dat <- function(raw_dat){
  cleaned_data <- raw_dat %>%
    select(Year, State, County, "Data Item", Value, "CV (%)")%>%
    rename("year" = "Year", "state"="State", "county"="County", 
           "data_item"= "Data Item", "value"="Value", "cv" = "CV (%)")%>%
    filter(state != "IDAHO")
  return(cleaned_data)
}

## Function to get state statistics
## Make state regions

# states <- unique(midwest$State)    for purposes of making north and south
north <- c("IDAHO","MINNESOTA","MONTANA", "NORTH DAKOTA",
           "SOUTH DAKOTA", "WISCONSIN")

mid <- c("ILLINOIS", "INDIANA", "IOWA", "NEBRASKA", "KANSAS",
         "KENTUCKY", "MISSOURI")

south <- c("ARKANSAS", "OKLAHOMA",
           "TENNESSEE", "TEXAS")


state_stats <- function(cln_dat){
  # get state stats 
  state_stats <- cln_dat %>%
    arrange(year,state)%>%
    group_by(year, state)%>%
    summarize(
      avg = mean(value),
      med = median(value),
      total = sum(value),
      max = max(value),
      min = min(value)
    )%>%
    mutate(
      region = case_when(
        state %in% north ~ "north",
        state %in% mid ~ "mid",
        state %in% south ~ "south"
      )
    )
  # Need to reorganize data to get the pct changes of each state
  pct_chgs <- distinct(state_stats, year, state, avg)%>%
    group_by(state)%>%
    mutate(
      pct_chg = (avg/lag(avg)-1)*100
    )
  state_stats <- merge(state_stats, pct_chgs)
  
  # get national average and % change of each year
  nat_stats <- state_stats%>%
    group_by(year)%>%
    summarize(
      nat_avg = mean(avg)
    )%>%
    mutate(
      nat_pct_chg = (nat_avg/lag(nat_avg) - 1) * 100
    )

  # once national trends are known, get % diff from national avg 
  state_stats <- merge(state_stats, nat_stats) %>%
    mutate(
      pct_diff = ((avg - nat_avg)/ nat_avg) * 100,
    )%>%
    select(year,state, region, avg, med, total, max,
           min, pct_chg, nat_avg,
           pct_diff, nat_pct_chg)

  #return(state_stats)
  return(state_stats)
}


sale_stats <- function(cln_dat){
  state_stats <- cln_dat %>%
    mutate(
      region = case_when(
        state %in% north ~ "north",
        state %in% mid ~ "mid",
        state %in% south ~ "south"
      )
    )%>%
    mutate(
      adj_value = adjust_for_inflation(value, from_date = year, "US", to_date = 2019)
    )%>%
    select(-c(county, cv))
  ## Adjust for inflation using CPI BLS 
  ## Source: https://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package
  
  return(state_stats)
}
## ---------------------------
## Clean Dataset of all midwest states from 1975-2020
midwest1 <- read_csv("./data/NASS_Ar_Kn_1975_2020_CY.csv")  # had to break it up
midwest2 <- read_csv("./data/NASS_Mn_ws_1975_2020_CY.csv")  # to download
midwest <- rbind(midwest1, midwest2)                        # recombine
cln_midwest <- cln_dat(midwest)

midwest_stats <- state_stats(cln_midwest)
head(filter(midwest_stats, state=="KANSAS"))
tail(midwest_stats, 5)


write.csv(midwest_stats, file= "./output/midwest_crop_yields.csv")

## Read in Crop sales data 
raw_sales <- read_csv("./data/NASS_1975_2020_sales.csv")
cln_sales <- cln_dat(raw_sales)

midwest_sales <- sale_stats(cln_sales)
write.csv(midwest_sales, file= "./output/midwest_sales.csv")

## ---------------------------
## Methods for standardizing clean data -- ENDED UP NOT USING -- 

## Standarize using stand_avg = (avg-min) / (max-min)
standardize <- function(cln_dat){
  # get standarized state stats 
  stand_stats <- cln_dat %>%
    arrange(year,state)%>%
    group_by(year, state)%>%
    summarize(
      avg = mean(value),
      max = max(value),
      min = min(value)
    )%>%
    mutate(
      stand_avg = (avg-min) / (max-min),
      region = case_when(
        state %in% north ~ "north",
        state %in% south ~ "south"
      )
    )
  # Need to reorganize data to get the pct changes of each state
  pct_chgs <- distinct(stand_stats, year, state, stand_avg)%>%
    group_by(state)%>%
    mutate(
      stand_pct_chg = (stand_avg/lag(stand_avg)-1)*100
    )
  stand_stats <- merge(stand_stats, pct_chgs)
  # get standardized national average and % change of each year
  nat_stats <- stand_stats%>%
    group_by(year)%>%
    summarize(
      min_state = min(avg),
      max_state = max(avg),
      nat_avg = mean(avg)
    )%>%
    mutate(
      stand_nat_avg = (nat_avg-min_state) / (max_state-min_state),
      stand_nat_pct_chg = (stand_nat_avg/ lag(stand_nat_avg) -1) *100,
    )
  
  # once national trends are known, get % diff from national avg 
  stand_stats <- merge(stand_stats, nat_stats) %>%
    mutate(
      stand_pct_diff = ((stand_avg - stand_nat_avg)/ stand_nat_avg) * 100,
    )%>%
    select(year,state, region, stand_avg, stand_pct_chg, stand_nat_avg,
          stand_pct_diff, stand_nat_pct_chg)
  
  
  return(stand_stats)
} 


stan_stats <- standardize(cln_midwest)
head(filter(stan_stats, state=="KANSAS"))
write.csv(stan_stats, file= "./output/standardized_crop_yields.csv")


