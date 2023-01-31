# Assignment 2b: Replicating Pendakur & Pendakur (2011)

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(haven)
library(quantreg)
library(modelsummary)

# Load data (2016 Individual Microdata for the Canadian census)

census_raw <-
  read_dta('assignment2-aboriginal/data/Census_2016_Individual_PUMF.dta')

# Cleaning the data ---------------------------------------------------------------------------------------

# I put NAs for all missing or not available values

# I also relevel the age, marital_statusd- factor to have the reference group that I want 

df_pre <-
  census_raw %>% 
  mutate(
    income = case_when(
      EmpIn %in% c(88888888, 99999999) ~ as.numeric(NA),
      TRUE ~ EmpIn
    ),
    agegrp = case_when(
      agegrp == 88 ~ as.double(NA),
      TRUE ~ as.double(agegrp)
    ),
    age = as.factor(agegrp) %>% relevel(9),
    marstat = as.factor(MarStH) %>%  relevel(1),
    hhsize = case_when(
      hhsize == 8 ~ as.double(NA),
      TRUE ~ as.double(hhsize)
    ),
    size = as.factor(hhsize) %>% relevel(1),
    kol = case_when(
      kol == 8 ~ as.double(NA),
      TRUE ~ as.double(kol)
    ),
    lang = as.factor(kol) %>% relevel(1),
    cma = as.factor(cma) %>% relevel('999'),
    education = case_when(
      hdgree %in% c(88,99) ~ as.double(NA),
      TRUE~ as.double(hdgree)
      ),
    education = as.factor(education) %>% relevel(2),
    aboid = aboid * 100
  )


# Create the "race" variables which includes all categories of indians

# First remove the NAs

# Then do all of my categorizations

df <- 
  df_pre %>%
  filter(ethder != 88) %>% 
  mutate(
    race = case_when(
      ethder != 1 ~ ethder,
      ethder == 1 & regind == 1 ~ as.double(999), # 999 is Registered Indians,
      ethder == 1 & regind == 0 & aboid != 6 ~ aboid,
      ethder == 1 & regind == 0 & aboid == 6 ~ as.double(998) # 998 is Ancestry
    )
  )

# Now redo, with labels

df <-
  df %>% 
  mutate(race = 
    case_when(
      race == 999 ~ 'Registered Indian',
      race == 100 ~ 'North American Indian',
      race == 200 ~ 'Métis',
      race == 300  ~ 'Inuit',
      race == 400  ~ 'Multiple',
      race == 500  ~ 'Other aboriginal response',
      race == 600 ~ 'Ancestry',
      TRUE ~ as.character(ethder),
    ),
    race = as.factor(race) %>% relevel('4')
  )

# Reducing the sample -------------------------------------------------------------------------------------

# We filter out people who are not employed by someone else.

df <-
  df %>% 
  filter(cow == 1,
         income > 0)

# Samples for Regressions ---------------------------------------------------------------------------------

# We always do different samples, one for women and another for men

# Whole of Canada

canada_men <-
  df %>% 
  filter(Sex == 2)

canada_women <-
  df %>% 
  filter(Sex == 1)

# Montreal

montreal_men <-
  df %>% 
  filter(cma == 462,
         Sex == 1)

montreal_women <-
  df %>% 
  filter(cma == 462,
         Sex == 2)

# Toronto

toronto_men <-
  df %>% 
  filter(cma == 535,
         Sex == 1)

toronto_women <-
  df %>% 
  filter(cma == 535,
         Sex == 2)

# Winnipeg

winnipeg_men <-
  df %>% 
  filter(cma == 535,
         Sex == 1)

winnipeg_women <-
  df %>% 
  filter(cma == 602,
         Sex == 2)

# Calgary

calgary_men <-
  df %>% 
  filter(cma == 825,
         Sex == 1)

calgary_women <-
  df %>% 
  filter(cma == 825,
         Sex == 2)

# Edmonton

edmonton_men <-
  df %>% 
  filter(cma == 835,
         Sex == 1)

edmonton_women <-
  df %>% 
  filter(cma == 835,
         Sex == 2)

# Vancouver

vancouver_men <-
  df %>% 
  filter(cma == 933,
         Sex == 1)

vancouver_women <-
  df %>% 
  filter(cma == 933,
         Sex == 2)

# Regressions ---------------------------------------------------------------------------------------------

# Canada

canada_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = canada_men,
     tau = c(0.2,0.5,0.8,0.9))

canada_women_reg <-
   rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
       data = canada_women,
      tau = c(0.2,0.5,0.8,0.9))

# Montreal

montreal_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = montreal_men,
     tau = c(0.2,0.5,0.8,0.9))

montreal_women_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = montreal_women,
     tau = c(0.2,0.5,0.8,0.9))

# Toronto

toronto_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = toronto_men,
     tau = c(0.2,0.5,0.8,0.9))

toronto_women_reg<-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = toronto_women,
     tau = c(0.2,0.5,0.8,0.9))

# Winnipeg

winnipeg_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = winnipeg_men,
     tau = c(0.2,0.5,0.8,0.9))

winnipeg_women_reg<-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = winnipeg_women,
     tau = c(0.2,0.5,0.8,0.9))

# Calgary

calgary_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = calgary_men,
     tau = c(0.2,0.5,0.8,0.9))

calgary_women_reg<-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = calgary_women,
     tau = c(0.2,0.5,0.8,0.9))

# Edmonton

edmonton_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = edmonton_women,
     tau = c(0.2,0.5,0.8,0.9))

edmonton_women_reg<-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = edmonton_women,
     tau = c(0.2,0.5,0.8,0.9))

# Vancouver

vancouver_men_reg <-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = vancouver_women,
     tau = c(0.2,0.5,0.8,0.9))

vancouver_women_reg<-
  rq(log(income) ~ race + marstat + age + hhsize + education + lang, 
     data = vancouver_women,
     tau = c(0.2,0.5,0.8,0.9))



