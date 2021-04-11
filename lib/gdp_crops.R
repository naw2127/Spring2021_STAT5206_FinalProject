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
## 
