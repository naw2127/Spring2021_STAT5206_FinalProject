## ---------------------------
##
## Script name: clean_work
##
## Purpose of script: Clean US census industry employment data 
##
## Author: Kimberly Li
##
## Date Created: 2021-03-30
##
## ---------------------------
##
## Notes: 
##
## ---------------------------

## Set working directory and load packages
kimberly_wd <- "~/Classes/DataScience/Spring2021_STAT5206_FinalProject"
setwd(kimberly_wd)     # Kimberly's working directory

library(tidyverse)

## Retrieve file names and paths
file_names <- list.files(path = "./data", pattern='*\\_EmploymentChange.csv')

file_paths <- paste0("data/", file_names)

## Function to read in and clean data (attach year, summarize by state)
read_clean <- function(file_name){
  raw <- read_csv(file_name)
  yr <- substr(file_name, 10, 13)
  Agriculture <- raw[raw$NAICS == 11 & raw$INIT_EMPLFL_N != "D" &
                       raw$NETCHG_EMPLFL_N != "D",]
  Agriculture %>% select(STATEDSCR, INIT_EMPL, NETCHG_EMPL, PCTCHG_EMPL) %>%
    group_by(STATEDSCR) %>% summarize(Net_Change = sum(NETCHG_EMPL)) %>%
    mutate(Year = yr) -> Agriculture
  return(Agriculture)
}

## Read in separate years and then combine into a dataframe
employ_changes_list <- lapply(file_paths, read_clean)

employ_changes_df <- as.data.frame(do.call(rbind, employ_changes_list))


## Graph comparing Oklahoma, North Dakota, and Indiana
IA_ND_OK <- employ_changes_df[employ_changes_df$STATEDSCR %in%
                                c("Oklahoma","North Dakota","Indiana"),]

g2 <- ggplot(IA_ND_OK, aes(x=Year, y=Net_Change, color=STATEDSCR)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = str_wrap("Net change amt of people employed in agriculture 2008 to 2016"),
    x = "Year",
    y = "Net Change in Employment"
  ) +
  theme_classic()+
  theme(legend.position = 'bottom')

g2
ggsave(path = "./figs", filename = "fig2.png")