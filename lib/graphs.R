## ---------------------------
##
## Script name: gdp_graphs
##
## Purpose of script: Create cleaner graphs of gdp
##
## Author: Kimberly Li
##
## Date Created: 2021-04-14
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

# Work directory and packages
kimberly_wd <- "~/Classes/DataScience/Spring2021_STAT5206_FinalProject"
setwd(kimberly_wd)     # Kimberly's working directory

natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd) 

library(tidyverse)

# Load data
gdp_stats<- read_csv("./output/GDP_stats.csv")
SIC_all_stats <- read_csv("./output/all_vars_1975_1997.csv")
NAICS_all_stats <- read_csv("./output/all_vars_2001_2019.csv")

# North and south
south <- c("OKLAHOMA", "ARKANSAS", "TENNESSEE", "TEXAS")
  
north <- c("WISCONSIN", "MINNESOTA", "MONTANA", "NORTH DAKOTA", "SOUTH DAKOTA")


head(gdp_stats)

no_mid_SIC <- SIC_all_stats[SIC_all_stats$state %in% north | SIC_all_stats$state %in% south,]
no_mid_NAICS <- NAICS_all_stats[NAICS_all_stats$state %in% north | NAICS_all_stats$state %in% south,]

gdp_raw_perc_SIC <- ggplot(no_mid_SIC,
                          aes(x = agri_gdp, y = perc_agri_gdp, group = state, color = state)) +
  facet_wrap(~region) +
  geom_smooth() + geom_point() +
  labs(title = "How",
       subtitle = "The southern states have a more consistent relationship between raw agricultural GDP and percent contribution",
       x = "Raw GDP (Millions of USD)",
       y = "Percent of GDP based on agriculture")
  
ggplot(no_mid_NAICS, aes(x = year, y = agri_gdp, group = state, color = state)) +
  facet_wrap(~region) + geom_smooth() + theme(legend.position = "bottom") +
  labs(title = "GDP from agriculture has been taking a nosedive since 2013",
       y = "raw GDP from agriculture (millions of USD)")

ag_output_1960_2004 <- read_csv("./output/total_agricultural_output_1960_2004.csv")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

no_mid_NAICS %>%
  mutate(outlier = ifelse(is_outlier(agri_gdp), state, as.numeric(NA))) %>%
  ggplot(aes(x = region, y = agri_gdp, fill = region)) +
  geom_boxplot() + scale_y_continuous(label = scales::unit_format(unit = "B", scale = 1e-6, sep = ""))+ coord_flip() +  
  labs(title = "Comparison of agricultural GDP distribution (2001-2019)",
       subtitle = "The south has a larger right skew, mostly due to Texas",
       x = "Region", y = "Raw Agricultural GDP in current dollars USD") +
  annotate("text", label = "Texas", x = 2.1, y = 1*10000000) +
  annotate("text", label = "Minnesota", x = 1.1, y = .87*10000000) ->
  GDP_box_2001_2019
GDP_box_2001_2019

ggsave(path = "./figs/final", filename = "GDP_box_2001_2019.png")

no_mid_SIC %>%
  mutate(outlier = ifelse(is_outlier(agri_gdp), state, as.numeric(NA)))%>%
  ggplot(aes(x = region, y = agri_gdp, fill = region)) +
  geom_boxplot() + scale_y_continuous(label = scales::unit_format(unit = "B", scale = 1e-3, sep = ""))+ coord_flip() +
  labs(title = "Comparison of agricultural GDP distribution (1975-1997)",
       subtitle = "Texas behaves very different then other southern states",
       x = "Region", y = "Raw Agricultural GDP in current dollars USD") +
  annotate("text", label = "Texas", x = 2.1, y = 7000) ->
  GDP_box_1975_1997
GDP_box_1975_1997
ggsave(path = "./figs/final", filename = "GDP_box_1975_1997.png")

#geom_text(aes(label = tolower(outlier)), na.rm = TRUE, nudge_x = .1)

no_mid_SIC %>% ggplot(aes(x = region, y = total_cy, fill = region)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Comparison of total crop yield (1975-1997)",
       x = "Region", y = "Total crop yield in summed bushels per acre") ->
  CY_box_1975_1997
CY_box_1975_1997

ggsave(path = "./figs/review", filename = "CY_box_1975_1997.png")

no_mid_NAICS %>% ggplot(aes(x = region, y = total_cy, fill = region)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Comparison of total crop yield (2001-2019)",
       x = "Region", y = "Total crop yield in summed bushels per acre") ->
  CY_box_2001_2019

ggsave(path = "./figs/review", filename = "CY_box_2001_2019.png")

no_mid_SIC %>% ggplot(aes(x = region, y = med_cy, fill = region)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Comparison of median crop yield (1975-1997)",
       subtitle = "Measuring by median centers the south better, suggests lots of outliers",
       x = "Region", y = "Median crop yield in bushels per acre") ->
  CY_med_box_1975_1997

ggsave(path = "./figs/review", filename = "CY_med_box_1975_1997.png")

no_mid_NAICS %>% ggplot(aes(x = region, y = med_cy, fill = region)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Comparison of median crop yield (2001-2019)",
       subtitle = "Norths median crop yield has moved up more",
       x = "Region", y = "Median crop yield in bushels per acre") ->
  CY_med_box_2001_2019

ggsave(path = "./figs/review", filename = "CY_med_box_2001_2019.png")

no_mid_SIC %>% ggplot() +
  facet_wrap(~region) +
  geom_smooth(aes(x = year, y = perc_agri_gdp, color = state)) +
  theme(legend.position = "bottom") + 
  geom_smooth(aes(x = year, y = total_cy, color = state)) +

no_mid_SIC %>% ggplot(aes(x = year, y = total_cy, color = state)) +
  facet_wrap(~region) +
  geom_smooth() + theme(legend.position = "bottom") -> p2

# dual axis graphing in progress
no_mid_SIC %>% ggplot() + 
  geom_smooth(aes(x = year, y = perc_agri_gdp, color = state)) + 
  scale_y_continuous( 
    "Casualties* due to:", 
    sec.axis = sec_axis(~. *0.001,  
                        name="Aircraft passengers carried, bn",)
  ) + 
  geom_line(aes(x = year, y = total_cy, color = state)) + 
  labs(title = "Aircraft safety", subtitle="Worldwide") +
  facet_wrap(~region)
