## ---------------------------
##
## Script name: analyze_crops
##
## Purpose of script: Analyze crop yield data cleaned by clean_crops.R and 
## saved to ./output/XXX.csv, create figs and plots
##
## Author: Natalie Williams
##
## Date Created: 2021-04-03
##
## ---------------------------
##
## Notes: This script has been modified to only generate data frames 
## Functions for plotting are in graph_crops.R
## All functions from earlier versions were moved to bottom 
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory (mac)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)
library(usmap)       # for plotting kmeans resutls on map
library(broom)       # for getting slopes of trendlines

## ---------------------------
## Read in cleaned data from ./output/midwest_crop_yields.csv

midwest_crops <- read_csv("./output/midwest_crop_yields.csv")
head(midwest_crops, 5)


## ---------------------------
# Details https://www.rdocumentation.org/packages/broom/versions/0.7.5/topics/glance.lm

pct_diff_m <- midwest_cy %>%
  na.omit()%>%
  group_by(state) %>%
  do(glance(lm(avg ~ year, data = .)))

preview <- select(pct_diff_m, c(state, r.squared))%>%
  arrange(r.squared)
mean.r <- mean(preview$r.squared, na.rm= TRUE)
head(preview)
ggplot(preview, aes(x=r.squared))+
  geom_density()+
  geom_vline(xintercept = mean.r)

# R square's values are super low

## ---------------------------
## New approach - compare diff between pct_chg and nat_pct_chg

growth_diff <- midwest_cy%>%
  na.omit()%>%
  mutate(
    growth_diff = pct_chg- nat_pct_chg
  )%>%
  select(year,state,region, growth_diff)

## ---------------------------
## Graph of growth diff over time
g7 <- ggplot(growth_diff, aes(x=year, y=growth_diff, color=state))+
  geom_point(size=0.5)+
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-25,25))+
  facet_wrap(~region, ncol =2)+
  labs(
    title = "State's Growth Compared to National Average Growth", 
    subtitle = "Northern States have a smaller distribution, but this could be due to smaller group size",
    y= "Percent Change")

g7

## Question 1b: How can the relationship of year and national avg be correlated? 
linearMod <- lm(nat_avg ~ year, data=midwest_cy)  # build linear regression model on full data
print(linearMod)

## ---------------------------
## Question 2: Are there differences between states? 
## Clustering Model: K- means k = 2
## Tutorial; https://www.r-bloggers.com/2019/10/cluster-multiple-time-series-using-k-means/
set.seed(37847)
col_dat <- select(midwest_cy, c(year, state, avg)) # select columns used in clustering

# Prepare data for clustering
clust_dat <- col_dat %>%
  pivot_wider(
    names_from = year, 
    values_from = avg
  )    # each col is a year, each row is a state


scaled_dat <- scale(na.omit(select(clust_dat, -state)))  # scale data for model

# Make model
clusters <- kmeans(scaled_dat, 3)

centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster")

clust_dat <- clust_dat%>% 
  na.omit()%>%
  mutate(cluster = clusters$cluster)      # add cluster to data frame

# Prepare data for graphing 
clust_long <- clust_dat %>%
  pivot_longer(cols=c(-state, -cluster), names_to = "year", values_to = "avg")%>%
  mutate(year = as.numeric(year))


centers_long <- centers %>%
  pivot_longer(cols=-cluster, names_to="year", values_to= "avg")%>%
  mutate(year = as.numeric(year))


# States in cluster 1 and 2 
clust_1 <- unique(filter(clust_long, cluster ==1)$state)
clust_2 <- unique(filter(clust_long, cluster ==2)$state)
clust_3 <- unique(filter(clust_long, cluster ==3)$state)

## ---------------------------
## Make graph
g2 <- ggplot() +
  geom_line(data = clust_long, aes(y = avg, x = year, group = state), colour = "#82518c") +
  facet_wrap(~cluster, nrow = 1) + 
  labs(
    title = "Average Crop Yield of US States",
    y= "Corn Bushels per Acre")

g2
ggsave(path = "./figs", filename = "cluster_graph.png")

## ---------------------------
## Make US map 

png("./figs/clustermap_k3.png")  # for saving
p2 <- plot_usmap(data = clust_long, values = "cluster", color = "white") + 
  scale_fill_continuous(low = "yellow", high = "lightgreen", 
                        name = "Cluster", label = scales::comma,
                        na.value="lightgray") + 
  labs(title = "Results of KMeans Clustering", subtitle = "Clusters of states with similar Crop Yield Trends") +
  theme(legend.position = "right")

p2
dev.off()  # when this line is run, the png is saved

## ---------------------------
## Next steps

## ---------------------------
## Question 1: How are crop yields changing nationally? 
g1 <- ggplot(midwest_cy, aes(x=year, y=nat_avg))+
  geom_point()+
  geom_smooth()+
  labs(
    title = "How have National Corn Yields varied since 1975?",
    x= "Year",
    y= "Corn Bushels per Acre"
  )+
  theme_classic()

g1
ggsave(path = "./figs", filename = "national_cy.png")

## ---------------------------
## Question 1a: What is the distribution of the states % change?
g2 <- ggplot(na.omit(select(midwest_cy, c(year,state, pct_chg))), aes(x=pct_chg))+
  geom_density()+
  facet_wrap(~state)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(
    title = "Distribution of % Change from 1975-2020", 
    subtitle = "As expected, most states have a mode of 0% and are slightly right skewed")

g2
ggsave(path = "./figs", filename = "dist_of_pct_chg.png")

## ---------------------------
## Question 1b: Percent Change over time
g3_dat <- na.omit(select(midwest_cy, c(year,state,region, pct_chg)))
g3 <- ggplot(g3_dat, aes(x=year, y=pct_chg, color=state))+
  geom_point(size=0.5)+
  geom_smooth()+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_wrap(~region)+
  labs(
    title = "States % Change of Corn Yields from 1975 to 2020", 
    subtitle = "Northern States have a smaller distribution, but this could be due to smaller group size",
    y= "Percent Change")

g3
ggsave(path = "./figs", filename = "pct_chg_over_time.png")

## Question 1c: Percent Change over time another way to visualize
g4 <- ggplot(g3_dat, aes(x=year, y=pct_chg, color=state))+
  geom_line()+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_wrap(~region, nrow=2)+
  labs(
    title = "States % Change of Corn Yields from 1975 to 2020", 
    subtitle = "Northern States have a smaller distribution, but this could be due to smaller group size",
    y= "Percent Change")

g4
ggsave(path = "./figs", filename = "pct_chg_over_time_line.png")

## Question 1d: Percent Difference from National Average over time 
g5_dat <- na.omit(select(midwest_cy, c(year,state,region, pct_diff))) 

g5 <- ggplot(g5_dat, aes(x=year, y=pct_diff, color=state))+
  geom_point(size=0.5)+
  geom_smooth(se=FALSE)+
  geom_hline(yintercept = 0, linetype= "dashed", color="black", size=1.5)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_wrap(~region, ncol=2)+
  labs(
    title = "% Difference from National Average Corn Yield", 
    subtitle = "Differences in trends between regions are hard to glean")

g5
ggsave(path = "./figs", filename = "pct_diff.png")

## Question 1e: Percent Difference from National Average over time - trend lines

g6 <- ggplot(g5_dat, aes(x=year, y=pct_diff, color=state))+
  geom_point(size=0.5)+
  geom_smooth(method="lm", se=FALSE)+
  geom_hline(yintercept = 0, linetype= "dashed", color="black", size=1.5)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_wrap(~region, ncol=2)+
  labs(
    title = "% Difference from National Corn Yield", 
    subtitle = "Northern States have a smaller distribution, but this could be due to smaller group size",
    y= "Percent Difference from the National Average Corn Yield")

g6
ggsave(path = "./figs", filename = "pct_diff_trend_lines.png")

## Divide g6 into two plots for better legend reading
g6a <- ggplot(filter(g5_dat, region=="north"), aes(x=year, y=pct_diff, color=state))+
  geom_point(size=0.5)+
  geom_smooth(method="lm", se=FALSE)+
  geom_hline(yintercept = 0, linetype= "dashed", color="black", size=1.5)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(
    title = "% Difference from National Corn Yield", 
    subtitle = "Northern states are collectively trending upwards, except 
                       Wisconsin, however Wisconsin remains above the national average",
    y= "Percent Difference from the National Average Corn Yield"
  )

g6a
g6b <- ggplot(filter(g5_dat, region=="south"), aes(x=year, y=pct_diff, color=state))+
  geom_point(size=0.5)+
  geom_smooth(method="lm", se=FALSE)+
  geom_hline(yintercept = 0, linetype= "dashed", color="black", size=1.5)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(
    title = "% Difference from National Corn Yield", 
    subtitle = "Southern States except for Arkansas, Tennessee, and Missouri are trending downwards",
    y= "Percent Difference from the National Average Corn Yield"
  )

g6b
ggsave(path = "./figs", filename = "south_pct_diff.png")

 