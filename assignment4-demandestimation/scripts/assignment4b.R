### -- Assignment 4b: Seemingly Unrelated Regressions---Demand -- ###

# Daniel Sánchez
# ECON836
# Due March 8th, 2023

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(micEconAids)
library(systemfit)

# Load the data (from the last assignment)

load('assignment4-demandestimation/data/df.RData')

# Running AIDS --------------------------------------------------------------------------------------------

# I will need to compute expenditure shares, and I do so below:

total_expenditures <-
  matched_dataset %>% 
  filter(exp_type != 'Clothing',
         expenditure >= 0) %>% 
  group_by(household) %>% 
  summarise(total_exp = sum(expenditure))

# Left join to the other dataset

df <- 
  matched_dataset %>%
  filter(exp_type != 'Clothing',
         expenditure > 0) %>% 
  left_join(total_expenditures, by = c('household')) %>% 
  mutate(exp_share = (abs(expenditure)/abs(total_exp)))

# Verify expenditure shares across households sum up to one

df %>% 
  group_by(household) %>% 
  summarise(exp = sum(exp_share))

# We will need a wide form dataset to implement this. I will also only deal with prime-aged households (the household head)
# Using only three rooms, and male household heads. Only size 2.

df_wide <-
  df %>%
  pivot_wider(names_from = exp_type,
              values_from = c(exp_share, price, expenditure)) %>% 
  left_join(total_expenditures, by = 'household') %>% 
  filter(!is.na(price_Food),
         !is.na(price_Shelter),
         !is.na(price_Transportation))

# Define the arguments necessary to run the AIDS model

priceNames <- c('price_Food', 'price_Shelter', 'price_Transportation')

shareNames <- c('exp_share_Food', 'exp_share_Shelter', 'exp_share_Transportation')

# Run the AIDS model as follows:

# aids <- aidsEst(priceNames,
                #shareNames,
                #'total_exp',
                #data = as.matrix(df_wide))

# Run a SUR model, as per AIDS

eq1 <- log(exp_share_Food) ~ log(price_Food) + log(size) + log(rooms) + as.factor(year) + as.factor(province)
eq2 <- log(exp_share_Transportation) ~ log(price_Transportation) + log(size) + log(rooms) + as.factor(year) + as.factor(province)
eq3 <- log(exp_share_Shelter) ~ log(price_Shelter) + log(size) + log(rooms) + as.factor(year) + as.factor(province)

# I include some demographic characteristics as well as year and province fixed effects

# Run the SUR model:

aids <-
  systemfit(list(eq1, eq2, eq3),
            data = df_wide,
            method = 'SUR')

# Crawford Paper ------------------------------------------------------------------------------------------

# This will require me to create new price vectors from the GST increase

# I do so below, as well as estimate the consumption of quantities by dividing by price

# THis is an estimate of the "demand"

df_wide_crawford <-
  df_wide %>% 
  mutate(
    price_Transportn = price_Transportation*1.05,
    price_Foodn = price_Food*1.05,
    price_Sheltern = price_Shelter*1.05,
    q_Transport = expenditure_Transportation/price_Transportation,
    q_Food = expenditure_Food/price_Food,
    q_Shelter = expenditure_Shelter/price_Shelter,
    qn_Transport = q_Transport*price_Transportn,
    qn_Shelter = price_Sheltern*q_Shelter,
    qn_Food = price_Foodn * q_Food
  )

# Now, we implement crawford by doing a COLI calculation as per the formula

df_wide_crawford <-
  df_wide_crawford %>%
  mutate(
    col_increase = 
      ((price_Transportn * qn_Transport + price_Foodn*qn_Food + price_Sheltern*qn_Shelter)/
         (price_Transportation * q_Transport + price_Food*q_Food + price_Shelter*q_Shelter))
  )

# Estimate the average COL and report

average_col <-
  df_wide_crawford %>% 
  summarise(MeanCol = (col_increase %>% mean(na.rm = F)) - 100)

average_col_province_year <-
  df_wide_crawford %>% 
  group_by(province, year) %>% 
  summarise(MeanCol = (col_increase %>% mean(na.rm = F)) - 100)


