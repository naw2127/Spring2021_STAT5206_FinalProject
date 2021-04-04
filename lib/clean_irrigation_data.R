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
 
raw_data <- read_csv("./data/NASS_2000_2020_IR.csv")
colnames <- colnames(raw_data)

del_cols <- c("Program", "Zip Code", "Week Ending", "Geo Level","State ANSI","Ag District","Ag District Code","County ANSI","Region",
              "watershed_code","Domain Category", "Domain", "Watershed")

fil_data <- select(raw_data, -del_cols)%>%filter(State != "DELAWARE")# filtered_data removed unwanted columns
print(fil_data, n=3)

states <- unique(fil_data$State)

## ---------------------------
## Pivot data wider to separate rows for irrigated and non irrigated data
dat_item <- unique(fil_data$`Data Item`)
print(dat_item)

## fxn to replace `Data Item` with "non-irrigated" or "irrigated"
replace_type <- function(dat){
  rep_dat <- dat%>% 
  mutate_if(is.character, str_replace_all, 
            pattern = dat_item[1], replacement = "irrigated")%>%
    mutate_if(is.character, str_replace_all, 
              pattern = dat_item[2], replacement = "non_irrigated")%>%
    rename("year" = "Year", "state"="State", "county"="County", 
           "data_item"= "Data Item", "value"="Value", "cv" = "CV (%)")
  return(rep_dat)
}

rep_data <- replace_type(fil_data)


## Function to pivot data and calc state stats
pivot_grp_dat <- function(dat){
  pivoted <- dat %>%
    ## use values_fn = sum bc "other counties" rows cause error otherwise
    pivot_wider(names_from = "Data Item", values_from= Value, values_fn = sum)%>%
    select(-"CV (%)")%>%
    group_by(Year, State)%>%
    summarize(
      irrigated_sum = sum(irrigated),
      non_irrigated_sum = sum(non_irrigated, na.rm= TRUE), 
      total = sum(irrigated_sum, non_irrigated_sum, na.rm=TRUE), 
      pc_irr = irrigated_sum/ total, 
      pc_non_irr = non_irrigated_sum / total
    )
  return(pivoted)
}

agg_dat <- pivot_grp_dat(rep_data)
print(agg_dat, n=5)

## Write to output folder
write.csv(agg_dat, file= "./output/state_irrigation_data.csv")

## ---------------------------
## Make graph
g1 <- ggplot(agg_dat, aes(x=Year, y=pc_non_irr))+
  geom_smooth(aes(color=State))+
  labs(
    title = str_wrap("Percent of Crop Yield without Irrigation?", width=40),
    x= "Year",
    y= "Irrigated Corn Bushels per Acre"
  )+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(legend.position = 'bottom')

g1
ggsave(path = "./figs", filename = "non_irrigation_trends.png")
