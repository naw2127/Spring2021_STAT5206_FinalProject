## ---------------------------
##
## Script name: clean_gdp
##
## Purpose of script: Clean agriculture's contribution to gdp state by state
##
## Author: Kimberly Li
##
## Date Created: 2021-04-05
##
## ---------------------------
##
## Notes: The merged data set is questionable in accuracy
##
## ---------------------------

## Set working directory and load packages
kimberly_wd <- "~/Classes/DataScience/Spring2021_STAT5206_FinalProject"
setwd(kimberly_wd)     # Kimberly's working directory

library(tidyverse)

## Retrieve file names and paths
#GDP in THOUSANDS current USD by NAICS industries
GDP_USD_NAICS <- read_csv("data/CAGDP2__ALL_AREAS_2001_2019.csv")

#GDP in MILLIONS current USD by SIC industries
GDP_USD_SIC <- read_csv("data/SAGDP2S__ALL_AREAS_1963_1997.csv")

#Percent change GDP by NAICS industries
GDP_PERC_NAICS <- read_csv("data/CAGDP11__ALL_AREAS_2002_2019.csv")

## Region separating
north <- c("IDAHO","MINNESOTA","MONTANA", "NORTH DAKOTA",
           "SOUTH DAKOTA", "WISCONSIN")
south <- c("ARKANSAS","ILLINOIS", "INDIANA", "IOWA", "KANSAS",
           "KENTUCKY", "MISSOURI", "NEBRASKA", "OKLAHOMA",
           "TENNESSEE", "TEXAS")
regions <- c("New England", "Mideast", "Great Lakes", "Plains", "Southeast",
             "Southwest", "Rocky Mountain", "Far West")

## Function to read in and clean data (attach year, summarize by state)
GDP_USD_SIC[GDP_USD_SIC$IndustryClassification == "[01-02]" |
              GDP_USD_SIC$IndustryClassification == "[07-09]",] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() %>%
  group_by(Region, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) -> GDP_SIC_longer


## Graphing
library(ggplot2)

# SIC graph with industries farming and agriculture
SIC_graph <- ggplot(GDP_SIC_longer, aes(x = as.numeric(Year),
                                        y = avg_gdp, color = as.factor(Region), group = as.factor(Region))) +
  geom_point() + geom_smooth() +
  labs(
    title = str_wrap("GDP contributed by farming/agriculture by region"),
    x = "Year",
    y = "Average GDP"
  ) +
  theme_classic() +
  theme(legend.position = 'bottom')

# SIC graph with agriculture industries only
GDP_USD_SIC[GDP_USD_SIC$IndustryClassification == "[07-09]",] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() %>%
  group_by(Region, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) -> GDP_SIC_Agonly
levels(GDP_SIC_Agonly$Region) <- regions
GDP_SIC_Agonly <- GDP_SIC_Agonly[GDP_SIC_Agonly$Region != "Far West",]

SIC_graph2 <- ggplot(GDP_SIC_Agonly, aes(x = as.numeric(Year),
                                         y = avg_gdp, color = as.factor(Region), group = as.factor(Region))) +
  geom_point() + geom_smooth() +
  labs(
    title = str_wrap("GDP contributed by agriculuture by region"),
    x = "Year",
    y = "Average GDP"
  ) +
  theme_classic() +
  theme(legend.position = 'bottom')

SIC_graph2
ggsave(path = "./figs", filename = "rawGDP_agriculture.png")

# SIC graph with farming industries only
GDP_USD_SIC[GDP_USD_SIC$IndustryClassification == "[01-02]",] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() %>%
  group_by(Region, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) -> GDP_SIC_Farmonly
levels(GDP_SIC_Farmonly$Region) <- regions
GDP_SIC_Farmonly <- GDP_SIC_Farmonly[GDP_SIC_Farmonly$Region != "Far West",]

SIC_graph3 <- ggplot(GDP_SIC_Farmonly, aes(x = as.numeric(Year),
                                           y = avg_gdp, color = as.factor(Region), group = as.factor(Region))) +
  geom_point() + geom_smooth() +
  labs(
    title = str_wrap("GDP contributed by farming by region"),
    x = "Year",
    y = "Average GDP"
  ) +
  theme_classic() +
  theme(legend.position = 'bottom')

SIC_graph3
ggsave(path = "./figs", filename = "rawGDP_farming.png")

# NAICS graph of agricultural GDP
GDP_USD_NAICS[GDP_USD_NAICS$IndustryClassification == 11,] %>%
  pivot_longer("2001":"2019", names_to = "Year") %>% na.omit() -> temp

temp[temp$value != "(D)" & temp$value != "(NA)",] %>%
  group_by(Region, Year) %>%
  summarize(avg_gdp = mean(as.numeric(value))/1000) -> GDP_NAICS_long
levels(GDP_NAICS_long$Region) <- regions
GDP_NAICS_long <- GDP_NAICS_long[GDP_NAICS_long$Region != "Far West",]

NAICS_graph <- ggplot(GDP_NAICS_long, aes(x = as.numeric(Year),
                                          y = avg_gdp, color = as.factor(Region), group = as.factor(Region))) +
  geom_point() + geom_smooth() +
  labs(
    title = str_wrap("GDP contributed by agriculture (NAICS definition) by region"),
    x = "Year",
    y = "Average GDP"
  ) +
  theme_classic() +
  theme(legend.position = 'bottom')

# Combining SIC and NAICS agriculture
rbind(GDP_SIC_Agonly, GDP_NAICS_long) %>% mutate(Region = as.factor(Region),
                                                 Year = as.numeric(Year)) -> Merged
# Graph of SIC and NAICS merged
Merged_graph <- ggplot(Merged, aes(x = as.numeric(Year),
                                   y = avg_gdp, color = as.factor(Region), group = as.factor(Region))) +
  geom_point() + geom_smooth() +
  labs(
    title = str_wrap("GDP contributed by agriculture per region"),
    x = "Year",
    y = "Average GDP"
  ) +
  theme_classic() +
  theme(legend.position = 'bottom')

# Unused linear line calculations
region_lines <- function(data, r){
  data[data$Region == r,] %>% mutate(Year = Year - min(Year)) -> temp
  model <- lm(temp$avg_gdp ~ temp$Year)
  return(model)
}

model <- c()
for(r in 1:length(regions)){
  model[r] <- region_lines(Merged, regions[r])
  names(model[r]) <- regions[r]
}