## ---------------------------
##
## Script name: combine_data
##
## Purpose of script: To combine cleaned data sets into one file then write
## to the output file 
##
## Author: Natalie Williams
##
## Date Created: 2021-04-14
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory (pc)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)

## ---------------------------
## Read in cleaned data sets 

crops <- read_csv("./output/midwest_crop_yields.csv")
sales <- read_csv("./output/midwest_sales.csv")
gdp <- read_csv("./output/GDP_stats.csv")
SIC_stats <- read_csv("./output/SIC_stats.csv")
NAICS_stats <- read_csv("./output/NAICS_stats.csv")
all_agri <- read_csv("./output/total_agricultural_output_1960_2004.csv")

## ---------------------
## Check data and make sure all regions are there 

head(crops)
head(sales)
head(gdp)
head(SIC_stats)
head(NAICS_stats)

# Check regions   SIC_stats, gdp, SIC_stats dont have "mid" 
print(unique(gdp$region))
check_regions <- filter(gdp, region =='south')

print(unique(check_regions$state))  # Should only be ARKANSAS, OKLAHOMA, TENNESSEE, TEXAS

## ----------------------
## Make Data Frame of columns: year, state, region, agri_gdp, total_gdp, 
##                             perc_agri_gdp, adj_prod_dollars, total_crop_yield

## Mutate column types and names for merge 
sales <- sales%>% rename(adj_prod_usd = adj_value)

gdp <- gdp %>%
  mutate(
    state = toupper(state) 
  )%>%
  rename(
    agri_gdp = value, 
    total_gdp = total, 
    perc_agri_gdp = perc,
  )

NAICS_stats <- NAICS_stats %>%
  mutate(
    state = toupper(state), 
    year = as.numeric(year)
  )%>%
  rename(
    agri_gdp = value, 
    total_gdp = total, 
    perc_agri_gdp = perc,
  )%>% select(-avg_gdp)


SIC_stats <- SIC_stats %>%
  mutate(
    state = toupper(state), 
    year = as.numeric(year)
  )%>%
  rename(
    agri_gdp = value, 
    total_gdp = total, 
    perc_agri_gdp = perc,
  )%>% select(-avg_gdp)

crops <- crops %>%
  rename(
    avg_cy = avg, 
    med_cy = med, 
    total_cy = total, 
    max_cy = max, 
    min_cy = min, 
    pct_chg_cy = pct_chg, 
    nat_avg_cy = nat_avg, 
    pct_diff_cy = pct_diff, 
    nat_pct_chg_cy = nat_pct_chg
  )


##-----------------
## Create Data sets from prepped data sets
combined_early <- inner_join(SIC_stats, crops, by = c("state", "year", "region"))%>%
  inner_join(., sales, by = c("state", "year", "region"))%>%
  left_join(., all_agri, by = c("state", "year", "region"))%>%
  select(-starts_with("X1"))

write.csv(combined_early, file="./output/all_vars_1975_1997.csv")

combined_late <- inner_join(NAICS_stats, crops, by = c("state", "year", "region"))%>%
  inner_join(., sales, by = c("state", "year", "region"))%>%
  left_join(., all_agri, by = c("state", "year", "region"))%>%
  select(-starts_with("X1"))

write.csv(combined_late, file="./output/all_vars_2001_2019.csv")