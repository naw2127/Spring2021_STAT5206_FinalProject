## ---------------------------
##
## Script name: gdp_crops
##
## Purpose of script: Analyze crop and GDP data together
##
## Author: Natalie Williams 
##
## Date Created: 2021-04-11
##
## ---------------------------
##
## Notes: SIC and NAICS stats are broken up because metrics used to measure 
## agriculture changed
##
## ---------------------------

## Set working directory and load packages
kimberly_wd <- "~/Classes/DataScience/Spring2021_STAT5206_FinalProject"
setwd(kimberly_wd)     # Kimberly's working directory
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd) 

library(tidyverse)

## ---------------------------
## Read in cleaned data sets 
crops <- read_csv("./output/midwest_crop_yields.csv")
sales <- read_csv("./output/midwest_sales.csv")
gdp <- read_csv("./output/GDP_stats.csv")
SIC_stats <- read_csv("./output/SIC_stats.csv")
NAICS_stats <- read_csv("./output/NAICS_stats.csv")
  
## ---------------------
## Check data make sure all regions are there 

head(crops)
head(sales)
head(gdp)
head(SIC_stats)
head(NAICS_stats)

# Check regions   SIC_stats, gdp, SIC_stats dont have "mid" 
print(unique(gdp$region))
check_regions <- filter(gdp, region =='south')

print(unique(check_regions$state))  # Should only be ARKANSAS, OKLAHOMA, TENNESSEE, TEXAS

## ---------------------
## Some prelim graphs 

## Stats with g % GDP from agriculture 
NAICS_stats_2019 <- NAICS_stats %>% filter(year == 2019)

head(NAICS_stats)

png("./figs/regions_map.png")  # for saving
p2 <- plot_usmap(data = NAICS_stats_2019, values = "perc", color = "white")+ 
  scale_fill_gradient(low =  "lightgreen", high =  "yellow", na.value="lightgray") +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing"
  )+
  theme(legend.position = "right")

p2
dev.off() 
