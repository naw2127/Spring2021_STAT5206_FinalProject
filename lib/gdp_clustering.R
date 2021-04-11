## ---------------------------
##
## Script name: gdp_clustering
##
## Purpose of script: Preparation and clustering of GDP data
##
## Author: Kimberly Li
##
## Date Created: 2021-04-05
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

library(tidyverse)
library(e1071)
library(usmap) 

# Subsetting out all states of interest, and general cleaning including:
# - Removing all na equivalent variables
# - Finding total gdps
# - Renaming columns
# - Merging the totals and finding percent contribution from agriculture
interest_states <- c(north, mid, south)

GDP_USD_SIC[!is.na(GDP_USD_SIC$Region) & toupper(GDP_USD_SIC$GeoName) %in% interest_states,] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() -> temp

temp[temp$Description == "All industry total",] %>%
  rename("state" = "GeoName", "year" = "Year", "total" = "value") %>%
  select(state, year, total) %>% mutate(total = as.numeric(total)) -> totals_SIC

GDP_USD_SIC[GDP_USD_SIC$IndustryClassification == "A" &
              !is.na(GDP_USD_SIC$Region) & toupper(GDP_USD_SIC$GeoName) %in% interest_states,] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year", "value" = "avg_gdp") -> GDP_SIC_State

GDP_USD_NAICS[!is.na(GDP_USD_NAICS$Region) &
                toupper(str_replace_all(GDP_USD_NAICS$GeoName,"[^[:graph:]]", " ")) %in% interest_states &
                GDP_USD_NAICS$Description == "All industry total",] %>%
  pivot_longer("2001":"2019", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(total = sum(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year") -> totals_NAICS

GDP_USD_NAICS[GDP_USD_NAICS$IndustryClassification == 11 &
                !is.na(GDP_USD_NAICS$Region) & toupper(str_replace_all(GDP_USD_NAICS$GeoName,"[^[:graph:]]", " ")) %in% interest_states &
                GDP_USD_NAICS$Description != "All industry total" &
                GDP_USD_NAICS$Description != "Private industries",] %>%
  pivot_longer("2001":"2019", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year", "value" = "avg_gdp") -> GDP_NAICS_State


inner_join(GDP_NAICS_State, totals_NAICS) %>% mutate(avg_gdp = value, perc = avg_gdp/total) ->
  NAICS_Perc
inner_join(GDP_SIC_State, totals_SIC) %>% mutate(avg_gdp = value, perc = avg_gdp/total) ->
  SIC_Perc


north <- c("IDAHO","MINNESOTA","MONTANA", "NORTH DAKOTA",
           "SOUTH DAKOTA", "WISCONSIN")

mid <- c("ILLINOIS", "INDIANA", "IOWA", "NEBRASKA", "KANSAS",
         "KENTUCKY", "MISSOURI")

south <- c("ARKANSAS", "OKLAHOMA",
           "TENNESSEE", "TEXAS")

# A very slightly modified version of Natalie's state cleaning code
state_stats <- function(cln_dat){
  # get state stats 
  state_stats <- cln_dat %>%
    mutate(
      region = case_when(
        toupper(state) %in% north ~ "north",
        toupper(state) %in% mid ~ "mid",
        toupper(state) %in% south ~ "south"
      )
    )
  "# get national average of each year
  nat_stats <- state_stats%>%
    group_by(year)%>%
    summarize(
      nat_avg_gdp = mean(avg_gdp),
      nat_avg_perc = mean(avg_perc)
    )
  
  # once national trends are known, get % diff from national avg 
  state_stats <- merge(state_stats, nat_stats) %>%
    mutate(
      pct_diff = (avg_gdp - nat_avg_gdp)/ nat_avg_gdp
    )
  "
  return(state_stats)
}

## GDP stats both raw and percent based (uncomment first, comment second for raw)
#GDP_stats <- state_stats(rbind(GDP_SIC_State, GDP_NAICS_State))
GDP_stats <- state_stats(rbind(NAICS_Perc, SIC_Perc))
SIC_stats <- state_stats(SIC_Perc)
NAICS_stats <- state_stats(NAICS_Perc)
NAICS_stats %>% select(year, state, avg_perc) %>%
  pivot_wider(names_from = year, values_from = avg_perc) -> clust_dat

## ---------------------
## Write cleaned/ analysed data to ./output/

write.csv(NAICS_stats, file= "./output/NAICS_stats.csv")

write.csv(SIC_stats, file= "./output/SIC_stats.csv")

write.csv(GDP_stats, file= "./output/GDP_stats.csv")

## ---------------------
## Scale data for clustering

scale(na.omit(select(clust_dat, -state))) -> scaled_dat

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

### Percent based only recent years
#png("./figs/gdpcluster_k3.png")  # for saving
#p2 <- plot_usmap(data = clust_long, values = "cluster", color = "white") + 
#  scale_fill_continuous(low = "yellow", high = "lightgreen", 
#                        name = "Cluster", label = scales::comma,
#                        na.value="lightgray") + 
#  labs(title = "Results of KMeans Clustering", subtitle = "Clusters of states with similar agricultural contribution to GDP trends") +
#  theme(legend.position = "right")
#
#p2
#dev.off()

### Percent based with break in data
png("./figs/percclusterNAICS_k3.png")  # for saving
p2 <- plot_usmap(data = clust_long, values = "cluster", color = "white") + 
  scale_fill_continuous(low = "yellow", high = "lightgreen", 
                        name = "Cluster", label = scales::comma,
                        na.value="lightgray") + 
  labs(title = "Results of KMeans Clustering", subtitle = "Clusters of states with similar agricultural contribution to GDP trends (including break)") +
  theme(legend.position = "right")

p2
dev.off()

### Raw, only recent years
#png("./figs/gdpclusterraw_k3.png")  # for saving
#p2 <- plot_usmap(data = clust_long, values = "cluster", color = "white") + 
#  scale_fill_continuous(low = "yellow", high = "lightgreen", 
#                        name = "Cluster", label = scales::comma,
#                        na.value="lightgray") + 
#  labs(title = "Results of KMeans Clustering", subtitle = "Clusters of states with similar agricultural USD amount in GDP trends") +
#  theme(legend.position = "right")
#
#p2
#dev.off()

# North and south split GDP average and percent
region_gdp_SIC <- ggplot(data = SIC_stats, aes(y = avg_gdp, x = as.numeric(year), group = state, color = state)) +
  geom_smooth() +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average raw GDP contributed by agriculture",
       subtitle = "It has increased faster in the south before 2000",
       x = "Year", y = "Average GDP (Millions of dollars)")

region_gdp_SIC
ggsave(path = "./figs", filename = "rawGDP_northsouth.png")

region_perc_SIC <- ggplot(data = SIC_stats, aes(y = avg_perc, x = as.numeric(year), group = state, color = state)) +
  scale_y_continuous(labels= scales::percent)+
  geom_smooth() +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average percent GDP contributed by agriculture",
       subtitle = "The percent contribution converges more in the south",
       x = "Year", y = "Average percent")

region_perc_SIC
ggsave(path = "./figs", filename = "percGDP_northsouth.png")

region_gdp_NAICS <- ggplot(data = NAICS_stats, aes(y = avg_gdp, x = as.numeric(year), group = state, color = state)) +
  geom_smooth(se = TRUE) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average raw GDP contributed by agriculture",
       subtitle = "It dips for everybody around 2013 and south drops faster",
       x = "Year", y = "Average GDP (Thousands of dollars)")

region_gdp_NAICS
ggsave(path = "./figs", filename = "GDPnew_northsouth.png")

region_perc_NAICS <- ggplot(data = NAICS_stats, aes(y = avg_perc, x = as.numeric(year), group = state, color = state)) +
  scale_y_continuous(labels= scales::percent)+
  geom_smooth(se = TRUE) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average percent GDP contributed by agriculture",
       subtitle = "The more agriculture was already contributing, the faster it dropped",
       x = "Year", y = "Average percent")

region_perc_NAICS
ggsave(path = "./figs", filename = "percnew_northsouth.png")
