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

head(all_early) # SIC Data merged with every thing 
head(all_late) # NAICS
## ---------------------
## Some prelim graphs 
## Stats with % GDP from agriculture 

SIC_stats_1975 <- all_early %>% filter(year == 1975 & state != "Idaho" & state != "Montana")

heatmap <- rbind(SIC_stats_1975, NAICS_stats_2019)%>% 
  select(year, state, perc_agri_gdp)%>%
  na.omit()

png("./figs/final/perc_GDP_map_1975_2019.png")  # for saving

p2 <- plot_usmap(data = heatmap, values = "perc_agri_gdp", color = "white",)+ 
  scale_fill_gradient(low =  "lightgreen", high =  "darkgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  facet_grid(~year)+
  labs(
    title = "Change in % of GDP from Agriculture, Forestry, and Fishing from 1975 to 2019"
  )+
  theme(legend.position = "left")

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
OK_full_early <- all_early %>%  filter(state == "OKLAHOMA")

TX_full_early <- all_early %>%  filter(state == "TEXAS")

AR_full_early <- all_early %>%  filter(state == "ARKANSAS")

TN_full_early <- all_early %>%  filter(state == "TENNESSEE")

south_early <- do.call("rbind", list(OK_full_early, TX_full_early, AR_full_early, TN_full_early))

## ----------------
## Northern states

WI_full_early <- all_early %>%  filter(state == "WISCONSIN")

MN_full_early <- all_early %>%  filter(state == "MINNESOTA")

MT_full_early <- all_early %>%  filter(state == "MONTANA")

ND_full_early <- all_early %>%  filter(state == "NORTH DAKOTA")

SD_full_early <- all_early %>%  filter(state == "SOUTH DAKOTA")

north_early <- do.call("rbind", list(WI_full_early, MN_full_early, MT_full_early, ND_full_early, SD_full_early))

## ----------------
## Southern States late years
OK_full_late <- all_late %>%  filter(state == "OKLAHOMA")

TX_full_late <- all_late %>%  filter(state == "TEXAS")

AR_full_late <- all_late %>%  filter(state == "ARKANSAS")

TN_full_late <- all_late %>%  filter(state == "TENNESSEE")

south_late <- do.call("rbind", list(OK_full, TX_full, AR_full, TN_full))

## ----------------
## Northern states

WI_full_late <- all_late %>%  filter(state == "WISCONSIN")

MN_full_late <- all_late %>%  filter(state == "MINNESOTA")

MT_full_late <- all_late %>%  filter(state == "MONTANA")

ND_full_late <- all_late %>%  filter(state == "NORTH DAKOTA")

SD_full_late <- all_late %>%  filter(state == "SOUTH DAKOTA")

north_late <- do.call("rbind", list(WI_full_late, MN_full_late, MT_full_late, ND_full_late, SD_full_late))

## ----------------
## Plot changes in agricultural GDP and corn crop yields for OK and ND
coeff = 0.001

g1 <- ggplot(OK_full_late, aes(x=year))+
  geom_smooth( method = "loess", aes(y=agri_gdp), color="darkgreen") + 
  geom_line( aes(y=agri_gdp), color="darkgreen", linetype = "dashed") + 
  geom_line( aes(y=total_cy/ coeff), size = 2, color="lightgreen")  +
  scale_y_continuous(
    name = "Agricultural GDP in Billions USD",
    label = scales::unit_format(unit = "B", scale = 1e-6, sep = ""),
    sec.axis = sec_axis(~.*coeff, name="Total State Corn Crop Yields in Corn Bushels per Acre")
  )+
  labs(
    title = "Oklahoma Agricultural GDP and Corn Crop Yields", 
    subtitle = "Despite decreasing corn crop yields, agricultural GDP slowly increases."
  )+ 
  annotate("text", label = "Corn Crop Yields", x = 2004, y = 5.5*1000000)+ 
  annotate("text", label = "Agricultural GDP", x = 2004, y = 2.6*1000000)
g1
ggsave(path = "./figs/final", filename = "OK corn yields and agri gdp.png")

g2 <- ggplot(ND_full_late, aes(x=year))+
  geom_smooth( method = "lm", aes(y=agri_gdp), color="darkgreen") + 
  geom_line( aes(y=agri_gdp), color="darkgreen", linetype = "dashed") + 
  geom_line( aes(y=total_cy/ coeff), size = 2, color="lightgreen")  +
  scale_y_continuous(
    name = "Agricultural GDP in Billions USD",
    label = scales::unit_format(unit = "B", scale = 1e-6, sep = ""),
    sec.axis = sec_axis(~.*coeff, name="Total State Corn Crop Yields in Corn Bushels per Acre")
  )+
  labs(
    title = "North Dakota Agricultural GDP and Corn Crop Yields", 
    subtitle = "In the past 20 years, corn yields have been steady and agricultural GDP is increasing."
  )+ 
  annotate("text", label = "Corn Crop Yields", x = 2004, y = 5.5*1000000)+ 
  annotate("text", label = "Agricultural GDP", x = 2004, y = 2.6*1000000)
g2
ggsave(path = "./figs/final", filename = "ND corn yields and agri gdp.png")

## ------------------------------------------------------
## Plot changes in  perc agricultural GDP and corn crop yields for OK and ND
## DONT END UP USING BC PERC of GDP is sooo low
coeff2 = 100000

g3 <- ggplot(OK_full_late, aes(x=year))+
  geom_smooth( method = "loess", aes(y=perc_agri_gdp), color="darkgreen") + 
  geom_line( aes(y=perc_agri_gdp), color="darkgreen", linetype = "dashed") + 
  geom_line( aes(y=total_cy/ coeff2), size = 2, color="lightgreen")  +
  scale_y_continuous(
    name = "% of GDP from Agriculture",
    label = scales::percent,
    sec.axis = sec_axis(~.*coeff2, name="Total State Corn Crop Yields in Corn Bushels per Acre")
  )+
  labs(
    title = "Oklahoma Agricultural GDP and Corn Crop Yields", 
    subtitle = "Despite decreasing corn crop yields, agricultural GDP slowly increases."
  )+ 
  annotate("text", label = "Corn Crop Yields", x = 2004, y = 0.055)+ 
  annotate("text", label = "Agricultural GDP %", x = 2004, y = 0.026)
g3


g4 <- ggplot(ND_full_late, aes(x=year))+
  geom_smooth( method = "lm", aes(y=agri_gdp), color="darkgreen") + 
  geom_line( aes(y=agri_gdp), color="darkgreen", linetype = "dashed") + 
  geom_line( aes(y=total_cy/ coeff), size = 2, color="lightgreen")  +
  scale_y_continuous(
    name = "Agricultural GDP in Billions USD",
    label = scales::unit_format(unit = "B", scale = 1e-6, sep = ""),
    sec.axis = sec_axis(~.*coeff, name="Total State Corn Crop Yields in Corn Bushels per Acre")
  )+
  labs(
    title = "North Dakota Agricultural GDP and Corn Crop Yields", 
    subtitle = "In the past 20 years, corn yields have been steady and agricultural GDP is increasing."
  )+ 
  annotate("text", label = "Corn Crop Yields", x = 2004, y = 5.5*1000000)+ 
  annotate("text", label = "Agricultural GDP", x = 2004, y = 2.6*1000000)
g4

## ----------------
## Use GGally package to make correlatoin plots 
# Southern
cov_dat_south <- south_early%>%
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt_south<- ggpairs(cov_dat_south, columns = 2:6, ggplot2::aes(color= state))
cov_plt_south
ggsave(path = "./figs/final", filename = "cov_south_plot.png")

## ----------------
# Northern

cov_dat_north <- north_early%>%
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt_north <- ggpairs(cov_dat_north, columns = 2:6, ggplot2::aes(color= state))
cov_plt_north
ggsave(path = "./figs/final", filename = "cov_north_plot.png")

## ----------------
# For Regions
north_south_early <- rbind(north_early, south_early)

cov_dat <- north_south_early%>%
  select(c(state, region, year, agri_gdp, total_cy, adj_prod_usd, rel_crop_output))

cov_plt <- ggpairs(cov_dat, columns = 2:7, ggplot2::aes(color= region))
cov_plt

ggsave(path = "./figs/final", filename = "cov_regions_plot.png")

## ----------------
# Plot Heat map of change in % gdp

g4 <- ggplot(filter(north_south_early, region != "mid"), aes(x= year, y = perc_agri_gdp))+
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
