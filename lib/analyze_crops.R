## ---------------------------
##
## Script name: analyze_crops
##
## Purpose of script: Analyze crop yield data cleaned by clean_crops.R and 
## saved to ./output/XXX.csv
##
## Author: Natalie Williams
##
## Date Created: 2021-04-03
##
## ---------------------------
##
## Notes: K means defies hypothesis
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory (mac)

## ---------------------------

## load up the packages we will need: 
library(tidyverse)
library(e1071)       # for graphing and some simple model building
library(usmap)       # for plotting kmeans resutls on map

## ---------------------------
## Read in cleaned data from ./output/midwest_crop_yields.csv

midwest_cy <- read_csv("./output/midwest_crop_yields.csv")
head(midwest_cy, 5)


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

## Question 1a: How can the relationship of year and national avg be correlated? 
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
    y= "Corn Bushels per Acre") +
  scale_x_continuous(breaks = seq(from = 1975, to = 2020, by = 05))

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
 