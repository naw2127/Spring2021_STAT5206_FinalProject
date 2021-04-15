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
library(hrbrthemes)   # for graphing
library(GGally)
library(usmap)   

## ---------------------------
## Read in cleaned combined data
all_early <- read_csv("./output/all_vars_1975_1997.csv")
all_late <- read_csv("./output/all_vars_2001_2019.csv")

head(all_early)  
head(all_late)
## ---------------------
## Some prelim graphs 
## Stats with % GDP from agriculture 

SIC_stats_1975 <- all_early %>% filter(year == 1975 & state != "Idaho" & state != "Montana")

head(SIC_stats_1975)

png("./figs/final/perc_GDP_map_1975.png")  # for saving

p2 <- plot_usmap(data = SIC_stats_1975, values = "perc_agri_gdp", color = "white",)+ 
  scale_fill_gradient(low =  "lightgreen", high =  "darkgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing in 1975"
  )+
  theme(legend.position = "right")

p2
dev.off() 

## IN 2019
NAICS_stats_2019 <- all_late %>% filter(year == 2019 & state != "Idaho" & state != "Montana")
  

head(NAICS_stats_2019)

png("./figs/final/perc_GDP_map_2019.png")  # for saving

p3 <- plot_usmap(data = NAICS_stats_2019, values = "perc_agri_gdp", color = "white",)+ 
  scale_fill_gradient(low =  "yellow", high =  "lightgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing in 2019"
  )+
  theme(legend.position = "right")

p3
dev.off() 

## ----------------
## Southern States early years
OK_full <- all_early %>%  filter(state == "OKLAHOMA")

TX_full <- all_early %>%  filter(state == "TEXAS")

AR_full <- all_early %>%  filter(state == "ARKANSAS")

TN_full <- all_early %>%  filter(state == "TENNESSEE")

## ----------------
## Northern states

WI_full <- all_early %>%  filter(state == "WISCONSIN")

MN_full <- all_early %>%  filter(state == "MINNESOTA")

MT_full <- all_early %>%  filter(state == "MONTANA")

ND_full <- all_early %>%  filter(state == "NORTH DAKOTA")

SD_full <- all_early %>%  filter(state == "SOUTH DAKOTA")


## ----------------
## Plots 
coeff = 10

g1 <- ggplot(OK_full, aes(x=year))+
  geom_smooth( method = "loess", aes(y=agri_gdp), color="darkgreen") + 
  geom_line( aes(y=total_cy), size = 2, color="lightgreen")  +
  scale_y_continuous(
    name = "Agricultural GDP",
    sec.axis = sec_axis(~.*coeff, name="Total State Crop Yields")
  )+
  labs(
    title = "Oklahoma Agricultural GDP and Crop Yields", 
    subtitle = "Dark green line indicated Agricultural GDP, light green Crop Yields"
  )
g1

## ----------------
## Use GGally package to make correlatoin plots 
# Southern
south_full <- do.call("rbind", list(OK_full, TX_full, AR_full, TN_full))
                      
cov_dat_south <- south_full%>%
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt_south<- ggpairs(cov_dat_south, columns = 2:6, ggplot2::aes(color= state))
cov_plt_south
ggsave(path = "./figs/final", filename = "cov_south_plot.png")

## ----------------
# Northern

north_full <- do.call("rbind", list(WI_full, MN_full, MT_full, ND_full, SD_full))

cov_dat_north <- north_full%>%
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt_north <- ggpairs(cov_dat_north, columns = 2:6, ggplot2::aes(color= state))
cov_plt_north
ggsave(path = "./figs/final", filename = "cov_north_plot.png")

## ----------------
# For Regions
north_south_full <- rbind(north_full, south_full)

cov_dat <- north_south_full%>%
  select(c(state, region, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt <- ggpairs(cov_dat, columns = 2:7, ggplot2::aes(color= region))
cov_plt

ggsave(path = "./figs/final", filename = "cov_regions_plot.png")

## ----------------
# Plot Heat map of change in % gdp

g4 <- ggplot(filter(gdp, region != "mid"), aes(x= year, y = perc_agri_gdp))+
  geom_smooth(aes(color = state))+
  facet_grid(~region)+
  scale_y_continuous(labels = scales::percent)+
  labs(
    title = str_wrap("Agriculture, Forestry, and Fishing contribute less to states' GDP over time", width = 50),
    y = "Percent GDP from Agriculture, Forestry, and Fishing"
  )

g4
png("./figs/final/region gdp from agri.png")  # for saving

p2 <- plot_usmap(data = filter(gdp, year==2019), values = "perc_agri_gdp", color = "white",)+ 
  scale_fill_gradient(low =  "lightgreen", high =  "darkgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing in 2019"
  )+
  theme(legend.position = "right")

p2
dev.off() 

g5 <- ggplot(cov_dat, aes(y = agri_gdp, x = rel_crop_output))+
  geom_point(aes(color=state))+
  geom_smooth(data = filter(cov_dat, state != "TEXAS"), method = "lm", aes(color = region))+
  facet_grid(~region)+
  labs(
    title = "Agriculture GDP in Millons of USD", 
    subtitle = "Texan Agricultural GDP dwarfs all other states",
    x = "Total Crop Output Relative to Alabama 1995"
  )
g5
