## ---------------------------
##
## Script name: graph_crops
##
## Purpose of script: To make visualizations of cleaned and/ or analyzed 
## crop data
##
## Author: Natalie Williams
##
## Date Created: 2021-04-10
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory (mac)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)
library(usmap)       # for plotting kmeans resutls on map
library(GGally)      # for plotting covar matrices

## ---------------------------
## Read in data 

midwest_stats <- read_csv("./output/midwest_crop_yields.csv")
midwest_sales <- read_csv("./output/midwest_sales.csv")
midwest_rel_output <- read_csv("./output/total_agricultural_output_1960_2004.csv")


## ---------------------------
## Make US map showing regions

png("./figs/final/regions_map.png")  # for saving
p2 <- plot_usmap(data = midwest_stats, values = "region", color = "white")+ 
  scale_fill_manual(values = c(`north` = "green", `mid` = "grey55", `south` = "yellow"),
                    name = "Region", na.value="lightgray") +
  theme(legend.position = "right")

p2
dev.off()  # when this line is run, the png is saved

## ---------------------------
## Check if corn is actually a suitable tracer for state output
## Correlation matrix of total crop output and corn output by region 

# select only desired midwest_stats col

total_vs_corn <- inner_join(midwest_stats, midwest_rel_output, by = c("year", "state", "region"))%>%
  select(c(year, state, region, total, rel_crop_output))%>%
  rename(
    "corn_total_yield" = "total"
  )

# Get correlation coefficient matrix 
south_corr <- ggcorr(filter(total_vs_corn, region == "south"), 
       method= c("pairwise", "pearson"),
       label_round = 2, label = TRUE)# pairwise so states are compared against themselves, pearson bc linear relationship
south_corr

# Get correlation coefficient matrix 
north_corr <- ggcorr(filter(total_vs_corn, region == "north"), 
                     method= c("pairwise", "pearson"),
                     label_round = 2, label = TRUE)# pairwise so states are compared against themselves, pearson bc linear relationship
north_corr

## -------------------------
## Peak at distribution of state totals in north and south
early_years <- filter(midwest_stats, year < 1991)

medians <- early_years %>%
  group_by(year, region)%>%
  summarize(
    region_med = median(total)
  )
early_year_dist <- merge(early_years, medians)

g1 <- ggplot(early_year_dist, aes(x=total, color = region, fill = region))+
  geom_density(alpha = 0.3)+
  #geom_vline(aes(xintercept = region_med, color = region))+
  scale_x_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, accuracy = 0.1))+
  facet_wrap(~year)+
  labs(
    title = "Distribution of Total State Corn Yields over years 1975-1990", 
    subtitle = "Oddly not normally distributed",
    x = "Total Crop Yields in Thousands Bushels per Acre"
  )

g1
ggsave(path = "./figs", filename = "density of regions.png")
## -------------------------
## Distribution but with mid states omitted
north_south <- filter(midwest_stats, year < 1991 & region != 'mid')

medians <- north_south %>%
  group_by(year, region)%>%
  summarize(
    region_med = median(total)
  )
north_south_dist <- merge(north_south, medians)

g2 <- ggplot(north_south_dist, aes(x=total, color = region, fill = region))+
  geom_density(alpha = 0.3)+
  geom_vline(aes(xintercept = region_med, color = region))+
  scale_x_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, accuracy = 0.1))+
  facet_wrap(~year)+
  labs(
    title = "Distribution of Total State Corn Yields over years 1975-1990", 
    subtitle = "Oddly not normally distributed",
    x = "Total Crop Yields in Thousands Bushels per Acre"
  )

g2
ggsave(path = "./figs", filename = "density of regions.png")
## -------------------------
## Peak at trends of state totals over time 
g3_dat <- midwest_stats %>% filter(region != "mid")

g3 <- ggplot(g3_dat, aes(y = total, x = year,color = state))+
  geom_point(size=0.5)+
  geom_smooth(method = "gam")+
  scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, accuracy = 0.1))+
  facet_wrap(~region)+
  labs(
    title = "Northern States see more consistent upward trends than southern states", 
    y = "Total Corn Crop Yields in Thousands Bushels per Acre"
  )

g3
ggsave(path = "./figs/final", filename = "totals v years.png")
## -------------------------
## same plot as g3 but line plot

g4 <- ggplot(g3_dat, aes(y = total, x = year, color = state))+
  geom_line( line_type = 'dashed')+
  geom_smooth(method = "gam")+
  scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, accuracy = 0.1))+
  facet_wrap(~region)+
  labs(
    title = "Northern States see more consistent upward trends than southern states", 
    y = "Total Corn Crop Yields in Thousands Bushels per Acre"
  )

g4
ggsave(path = "./figs/final", filename = "totals v years with lines.png")

## -------------------------
## Plot sale data

g5 <- ggplot(midwest_sales , aes(y = adj_value, x = year,color = state))+
  geom_smooth(method = "gam")+
  facet_wrap(~region)+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.1))+
  labs(
    title = "Northern and southern states account for a small fraction of Corn Sales",
    subtitle = "Prices adjusted to 2019 Dollars Using World Bank Resources",
    y = "Total Corn Sales in $ "
  )

g5
ggsave(path = "./figs", filename = "Corn sale.png")

## -------------------------
## Plot sale data vs Crop Yield data

sale_crop_totals <- left_join(select(midwest_crops, c(year, state, region, total)),
                              select(midwest_sales, c(year, state, region, adj_value)))%>%
  arrange(total)%>% filter(region != "mid")

g6 <- ggplot(sale_crop_totals , aes(y = adj_value, x = total, color = state))+
  geom_smooth()+
  facet_wrap(~region)+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 1))+
  labs(
    title = "Despite having similar yields, southern states production is valued much lower ", 
    subtitle = "Prices adjusted to 2019 Dollars Using World Bank Resources",
    y = "Total Corn Production in $ ", 
    x = "Total Corn Yield in Bushels per Acre"
  )

g6

ggsave(path = "./figs", filename = "Corn production.png")

south_sale_crop_totals <- filter(sale_crop_totals, region== "south")

g7 <- ggplot(south_sale_crop_totals , aes(y = adj_value, x = total, color = state))+
  geom_smooth()+
  facet_wrap(~region)+
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 1))+
  labs(
    title = "", 
    subtitle = "Prices adjusted to 2019 Dollars Using World Bank Resources",
    y = "Total Corn Production in $ ", 
    x = "Total Corn Yield in Bushels per Acre"
  )

g7


## ----------------
## graph of rel crop outputs

g8 <- ggplot(midwest_rel_output, aes(x = year, y = rel_crop_output))+
  geom_line(aes(group = state, color = region), size= 1., linetype = "dashed")+
  geom_smooth(method = "lm", aes(color = region), size = 2)+
  labs(
    title = "Total Crop Output relative to 1996 Alabama",
    subtitle = "Northern states over take southern states in 1972",
    y = "Crop Output relative to 1996 Alabama"
  )+
  annotate("text", label = "Minnesota", x = 1990, y = 7)+
  annotate("text", label = "Texas", x = 1965, y = 5) 

g8

ggsave(path = "./figs", filename = "all crop output north and south.png")


