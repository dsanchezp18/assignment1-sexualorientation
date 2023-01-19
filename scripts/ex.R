
library(tidyverse) # For data wrangling
library(haven) # Reading stata files
library(labelled) # For survey labels
library(modelsummary) # For tables
library(data.table) # For combinations


df<-
  read_dta('data/Census_2016_Hierarchial.dta')


household <-
  df %>% 
  filter(agegrp %in% c(9,10,11)) %>% 
  group_by(HH_ID) %>% 
  summarise(people = n(),
            sexes = n_distinct(sex)) %>% 
  filter(people == 2) %>% 
  mutate(gay = case_when(
    people > sexes ~ 'Gay',
    TRUE ~ 'Not Gay'
  ))

df_for_table<-
  df %>%
  left_join(household, by = 'HH_ID') %>% 
  filter(people == 2)
