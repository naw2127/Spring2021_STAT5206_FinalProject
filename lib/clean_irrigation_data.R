## ---------------------------
##
## Script name: water_crops
##
## Purpose of script: To clean USDA crop yield data for Montana, Nebraska, Oklahoma
## North Dakota. Data contains crop yeilds of irrigated and non irrigated crops 
## in BU/ ACRE different states into one file
##
## Author: Natalie Williams
##
## Date Created: 2021-04-03
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

## set working directory 
natalie_wd <- "~/SENIOR YEAR/Stat comp and intro to data science/Final Project/Spring2021_STAT5206_FinalProject"
setwd(natalie_wd)     # Natalie's working directory 

## ---------------------------

## load up the packages we will need: 
library(tidyverse)

## ---------------------------
## Read in crop yield data 
 
raw_data <- read_csv("./data/NASS_1975_2020_Irrigation.csv")
colnames <- colnames(raw_data)


# Remove  unneccessary columns
del_cols <- c("Program", "Zip Code","County","CV (%)", "Week Ending", "Geo Level","State ANSI","Ag District","Ag District Code","County ANSI","Region",
              "watershed_code","Domain Category", "Domain", "Watershed")

fil_data <- select(raw_data, -del_cols)# filtered_data removed unwanted columns and states
print(fil_data, n=3)

## ---------------------------
## Rename columns, rename data item possible values, pivot wider add regions

north <- c("IDAHO","MINNESOTA","MONTANA", "NORTH DAKOTA",
           "SOUTH DAKOTA", "WISCONSIN")

mid <- c("ILLINOIS", "INDIANA", "IOWA", "NEBRASKA", "KANSAS",
         "KENTUCKY", "MISSOURI")

south <- c("ARKANSAS", "OKLAHOMA",
           "TENNESSEE", "TEXAS")

cln_dat <- function(raw_dat){
  
  cleaned_data <- raw_dat %>%
    select(Year, State, "Data Item", Value)%>%
    rename("year" = "Year", "state"="State", 
           "data_item"= "Data Item", "value"="Value" )%>%
    mutate(
      region = case_when(
        state %in% north ~ "north",
        state %in% mid ~ "mid",
        state %in% south ~ "south"
      )
    )%>%
    filter(!is.na(region))    # raw data includes states not needed in analysis
  
  # change possible string values in data_item col and pivot data
  piv_dat <- cleaned_data%>%
    mutate(
      data_item = if_else(grepl("NON", data_item), "non_irrigated", "irrigated")
    )%>%
    mutate(id = row_number())%>%
    pivot_wider(
      id_cols =c(id, state, year, region), names_from = data_item, values_from = value,
      values_fill = NA,
      )%>%
    select(-id)%>%
    arrange(non_irrigated)
  
  return(piv_dat)
}
cln_irr <- cln_dat(fil_data)
head(cln_irr)

## ----------------------------------
## Get counts of NA values 

na_counts <- cln_irr %>%
  group_by(state)%>%
  summarise(
    total_obs = n(),
    non_irrigated = (-1* sum(is.na(non_irrigated))),
    irrigated = sum(is.na(irrigated))
  )%>%
  pivot_longer(cols=c(non_irrigated, irrigated), names_to = "type", values_to = "count")

g1 <- ggplot(na_counts, aes(x = state, y = count, fill = type))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(
    title = "Number of Observations that are NA"
  )
g1
## --------------------------------------------
## Function to calc state stats
irr_stats <- function(cln_dat){
  stats <- cln_dat%>%
    group_by(year, state, region)%>%
    summarize(
      irrigated = sum(irrigated),
      non_irrigated = sum(non_irrigated),
      pct_irr = irrigated/ (irrigated + non_irrigated), 
      pct_non_irr = non_irrigated/ (irrigated + non_irrigated)
    )
  return(stats)
}

irr_stats <- irr_stats(cln_irr)
head(irr_stats)

## Write to output folder
write.csv(irr_stats, file= "./output/state_irrigation_data.csv")

## ---------------------------
## Make graph
g1 <- ggplot(cln_irr, aes(x=year, y=non_irrigated))+
  geom_point(aes(color = state))+
  labs(
    title = "Yield of Non Irrigated Corn in Bushels / Acre",
    x= "Year",
    y= "Bushels / Acre"
  )+
  theme(legend.position = 'bottom')

g1
ggsave(path = "./figs", filename = "non_irrigation_trends.png")
