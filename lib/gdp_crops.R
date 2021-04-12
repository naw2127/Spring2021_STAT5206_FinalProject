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

## Stats with % GDP from agriculture 

SIC_stats_1970 <- SIC_stats %>% filter(year == 1970)


head(SIC_stats_1970)

png("./figs/perc_GDP_map_1970.png")  # for saving

p2 <- plot_usmap(data = SIC_stats_1970, values = "perc", color = "white",)+ 
  scale_fill_gradient(low =  "lightgreen", high =  "darkgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing in 1970"
  )+
  theme(legend.position = "right")

p2
dev.off() 

## IN 2019
NAICS_stats_2019 <- NAICS_stats %>% filter(year == 2019)
  

head(NAICS_stats_2019)

png("./figs/perc_GDP_map_2019.png")  # for saving

p2 <- plot_usmap(data = NAICS_stats_2019, values = "perc", color = "white",)+ 
  scale_fill_gradient(low =  "yellow", high =  "lightgreen", na.value="lightgray",
                      name = "Percent GDP",label = scales::percent) +
  labs(
    title = "% of GDP from Agriculture, Forestry, and Fishing in 2019"
  )+
  theme(legend.position = "right")

p2
dev.off() 

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
combined_early <- inner_join(SIC_stats, crops)%>%
  inner_join(., sales, by = c("state", "year", "region"))

combined_late <- inner_join(NAICS_stats, crops)%>%
  inner_join(., sales, by = c("state", "year", "region"))

## ----------------
## Southern States early years
OK_full <- combined_early %>%
  filter(state == "OKLAHOMA")

TX_full <- combined_early %>%
  filter(state == "TEXAS")

AR_full <- combined_early %>%
  filter(state == "ARKANSAS")

TN_full <- combined_early %>%
  filter(state == "TENNESSEE")

## ----------------
## Northern states

WI_full <- combined_early %>%
  filter(state == "WISCONSIN")

MN_full <- combined_early %>%
  filter(state == "MINNESOTA")

MT_full <- combined_early %>%
  filter(state == "MONTANA")

ND_full <- combined_early %>%
  filter(state == "NORTH DAKOTA")

SD_full <- combined_early %>%
  filter(state == "SOUTH DAKOTA")


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
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd))

cov_plt_south<- ggpairs(cov_dat_south, columns = 2:5, ggplot2::aes(color= state))
cov_plt_south
ggsave(path = "./figs", filename = "cov_south_plot.png")

## ----------------
# Northern

north_full <- do.call("rbind", list(WI_full, MN_full, MT_full, ND_full, SD_full))

cov_dat_north <- north_full%>%
  select(c(state, year, agri_gdp, total_cy, adj_prod_usd))

cov_plt_north <- ggpairs(cov_dat_north, columns = 2:5, ggplot2::aes(color= state))
cov_plt_north
ggsave(path = "./figs", filename = "cov_north_plot.png")

## ----------------
# For Regions
north_south_full <- rbind(north_full, south_full)

cov_dat <- north_south_full%>%
  select(c(state, region, year, agri_gdp, total_cy, adj_prod_usd))

cov_plt <- ggpairs(cov_dat, columns = 2:6, ggplot2::aes(color= region))
cov_plt

ggsave(path = "./figs", filename = "cov_regions_plot.png")

