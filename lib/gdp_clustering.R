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
interest_states <- c(north, south)

GDP_USD_SIC[!is.na(GDP_USD_SIC$Region) & toupper(GDP_USD_SIC$GeoName) %in% interest_states,] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() -> temp

temp[temp$value != "(L)" & temp$value != "(NA)",] %>%
  group_by(GeoName, Year) %>% summarize(total = sum(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year") -> totals_SIC

GDP_USD_SIC[GDP_USD_SIC$IndustryClassification == "[07-09]" &
              !is.na(GDP_USD_SIC$Region) & toupper(GDP_USD_SIC$GeoName) %in% interest_states,] %>%
  pivot_longer("1963":"1997", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year", "value" = "avg_gdp") -> GDP_SIC_State

GDP_USD_NAICS[!is.na(GDP_USD_NAICS$Region) &
                toupper(str_replace_all(GDP_USD_NAICS$GeoName,"[^[:graph:]]", " ")) %in% interest_states,] %>%
  pivot_longer("2001":"2019", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(total = sum(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year") -> totals_NAICS

GDP_USD_NAICS[GDP_USD_NAICS$IndustryClassification == 11 &
              !is.na(GDP_USD_NAICS$Region) & toupper(str_replace_all(GDP_USD_NAICS$GeoName,"[^[:graph:]]", " ")) %in% interest_states,] %>%
  pivot_longer("2001":"2019", names_to = "Year") %>% na.omit() %>%
  group_by(GeoName, Year) %>% summarize(avg_gdp = mean(as.numeric(value))) %>%
  rename("state" = "GeoName", "year" = "Year", "value" = "avg_gdp") -> GDP_NAICS_State

inner_join(GDP_NAICS_State, totals_NAICS) %>% mutate(avg_gdp = value, value = avg_gdp/total) ->
  NAICS_Perc
inner_join(GDP_SIC_State, totals_SIC) %>% mutate(avg_gdp = value, value = avg_gdp/total) ->
  SIC_Perc

# A very slightly modified version of Natalie's state cleaning code
state_stats <- function(cln_dat){
  # get state stats 
  state_stats <- cln_dat %>%
    group_by(year, state)%>%
    summarize(
      avg = mean(value),
      max = max(value),
      min = min(value)
    )%>%
    mutate(
      region = case_when(
        toupper(state) %in% north ~ "north",
        toupper(state) %in% south ~ "south"
      )
    )
  # get national average of each year
  nat_stats <- state_stats%>%
    group_by(year)%>%
    summarize(
      nat_avg = mean(avg) 
    )
  
  # once national trends are known, get % diff from national avg 
  state_stats <- merge(state_stats, nat_stats) %>%
    mutate(
      pct_diff = (avg - nat_avg)/ nat_avg
    )
  
  return(state_stats)
}

## GDP stats both raw and percent based (uncomment first, comment second for raw)
#GDP_stats <- state_stats(rbind(GDP_SIC_State, GDP_NAICS_State))
GDP_stats <- state_stats(rbind(NAICS_Perc, SIC_Perc))
GDP_stats %>% select(year, state, avg) %>%
  pivot_wider(names_from = year, values_from = avg) -> clust_dat

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
png("./figs/gdpclusterBREAK_k3.png")  # for saving
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
