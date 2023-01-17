# Assignment 1b: Replicating *Sexual Orientation and Labor Economics* by Cara Brown (1998)

# Daniel Sanchez
# ECON836
# Due January 19th, 2023

# Preliminaries -------------------------------------------------------------------------------------------

# Libraries

library(tidyverse) # For data wrangling
library(haven) # Reading stata files
library(modelsummary) # For tables

# Load data with the haven package as it is in a .dta file

df<-
  read_dta('data/Census_2016_Hierarchial.dta')  # Alternatively, use read_stata()

# Data Cleaning -------------------------------------------------------------------------------------------

# Eliminate all ages we're not interested in and remove all missing values in variables of interest

df<-
  df %>% 
  filter(agegrp %in%  c(9,10,11),
         sex != 8,
         MarStH != 8,
         empin != 88888888,
         empin != 99999999) 

# Capturing the sample of presumed homosexuals ------------------------------------------------------------

# Create a new helper variable to see if they are married or common law

# These people either have children or not (two possible responses in the cfstat variable)

df <- 
  df %>% 
  mutate(married = case_when(
    cfstat %in% c(1,2) ~ 1,
    TRUE ~ 0
  ))

households_married <-
  df %>%
  filter(married == 1) %>% 
  group_by(HH_ID) %>% 
  summarise(sum_people = n(),
            count_sex = n_distinct(sex),
            cfstat_household = first(cfstat),
            count_cfstats = n_distinct(cfstat),
            n_families_econ  = n_distinct(EF_ID),
            n_families_census = n_distinct(CF_ID)) %>%
  filter(sum_people == 2) %>% 
  mutate(gay_household = ifelse(count_sex == 1, 1, 0)) %>% 
  arrange(desc(sum_people))

# Single gay people ---------------------------------------------------------------------------------------

df <-
  df %>% 
  mutate(single = ifelse(MarStH %in% c(1,4), 1, 0))

households_single <-
  df %>% 
  filter(cfstat == 7) %>% 
  group_by(HH_ID) %>% 
  summarise(sum_people = n(),
            count_sex = n_distinct(sex),
            cfstat_household = first(cfstat),
            count_cfstats = n_distinct(cfstat),
            n_families_econ  = n_distinct(EF_ID),
            n_families_census = n_distinct(CF_ID)) %>% 
  filter(sum_people == 2) %>% 
  mutate(gay_household = ifelse(count_sex == 1, 1, 0)) %>% 
  arrange(desc(sum_people))
  

# Gay Households ------------------------------------------------------------------------------------------

households_gay <-
  households_married %>% 
  bind_rows(households_single)

# Join the household level data to the individual dataset

df <-
  df %>% 
  left_join(households_gay, by = 'HH_ID') %>% 
  mutate(gay_household = ifelse(is.na(gay_household),0, gay_household)) %>% 
  rename(gay = 'gay_household')

# Table ---------------------------------------------------------------------------------------------------

df <-
  df %>% 
  mutate(gay = case_when(
    gay == 1 ~ 'Homosexual',
    gay == 0 ~ 'Heterosexual'
  ),
  agegrp = case_when(
    agegrp %in% c(9,10) ~ '45 to 54',
    agegrp == 11 ~ '55 to 64'
  ),
  sex = case_when(
    sex == 1 ~ 'Female',
    sex == 2 ~ 'Male'
  ),
  MarStH = case_when(
    MarStH %in% c(2,3) ~ 'Married or Common Law',
    MarStH == 1 ~ 'Single',
    MarStH == 4 ~ 'Separated or Widowed'
  )) %>% 
  rename(
    Income = 'empin',
    MarStatus = 'MarStH',
    Orientation = 'gay',
    Sex = 'sex',
    Age = 'agegrp'
    
  )

# Now make the table

datasummary(Income * Orientation * MarStatus ~ Mean*Sex*Age, 
            data = df,
            output = 'markdown')

# Robustness Checks ---------------------------------------------------------------------------------------

families_econ  <-
  df %>% 
  group_by(EF_ID) %>% 
  summarise(houses =  n_distinct(HH_ID),
            people = n(),
            sexes = n_distinct(sex)) %>% 
  arrange(desc(houses))

# Econ families necessarily live in only one house

families_census  <-
  df %>% 
  group_by(CF_ID) %>% 
  summarise(houses =  n_distinct(HH_ID),
            people = n(),
            sexes = n_distinct(sex)) %>% 
  arrange(desc(houses))



