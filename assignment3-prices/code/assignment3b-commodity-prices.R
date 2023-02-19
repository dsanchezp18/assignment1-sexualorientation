####################  Assignment 3b: Finding and matching data for Demand Analysis
# ECON836 Applied Econometrics
# Daniel Sanchez

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(haven)
library(tidyverse)
library(kableExtra)

# Load the data: SCS

scs_09 <-
  read_sav('assignment3-prices/data/shs2009.sav') %>% 
  mutate(year = 2009)

scs_08 <-
  read_sav('assignment3-prices/data/shs2009.sav')%>% 
  mutate(year = 2008)

scs_07<-
  read_sav('assignment3-prices/data/shs2009.sav') %>% 
  mutate(year = 2007)

scs_06 <-
  read_sav('assignment3-prices/data/shs2009.sav')%>% 
  mutate(year = 2006)

scs_05 <-
  read_sav('assignment3-prices/data/shs2009.sav')%>% 
  mutate(year = 2005)

# Load the data: CPI

cpi <-
  read.csv('assignment3-prices/data/cpi.csv')

# Preparing the SHS ---------------------------------------------------------------------------------------

# Binding all years

scs <- 
  scs_09 %>%
  bind_rows(scs_08) %>% 
  bind_rows(scs_07) %>% 
  bind_rows(scs_06) %>% 
  bind_rows(scs_05)

# Renaming variables 

scs <-
  scs %>% 
  rename(Food = 'F001',
         Shelter = 'G001',
         Transportation = 'K001',
         Clothing = 'J001',
         province = 'PROVINCP',
         size = 'HHSZTOTP',
         household = 'CASEID',
         rooms = 'NUMRMP',
         type = 'TYPDWELP',
         tenure = 'TENURYRP',
         age_reference = 'RPAGEGRP',
         sex_reference = 'RPSEX')

# Grouping at the province per year per household (filter for only full year)

scs_grouped_mean <-
  scs %>% 
  filter(FYPYFLAG == 1,
         province != 0) %>% 
  group_by(year, province, size) %>% 
  summarise(
    Food = mean(Food, na.rm = T),
    Shelter = mean(Shelter, na.rm = T),
    Transportation = mean(Transportation, na.rm = T),
    Clothing = mean(Clothing, na.rm = T)
  )

scs_grouped_sd <-
  scs %>% 
  filter(FYPYFLAG == 1,
         province != 0) %>% 
  group_by(year, province, size) %>% 
  summarise(
    Food.Sd = sd(Food, na.rm = T),
    Shelter.Sd = sd(Shelter, na.rm = T),
    Transportation.Sd = sd(Transportation, na.rm = T),
    Clothing.Sd = sd(Clothing, na.rm = T)
  )

scs_grouped <-
  scs_grouped_mean %>% 
  left_join(scs_grouped_sd, by = c('year','province', 'size'))

# Preparing the CPI ---------------------------------------------------------------------------------------

# Cleaning

cpi_to_match <-
  cpi %>%
  rename(exp_type = 'Products.and.product.groups',
         price = 'VALUE',
         year = 'REF_DATE') %>% 
  filter(exp_type %in% c('Food','Shelter','Transportation','Clothing'),
         GEO != 'Canada') %>%
  mutate(province = case_when(
    GEO == 'Newfoundland and Labrador' ~ 10,
    GEO =='New Brunswick' ~ 13,
    GEO =='Quebec' ~ 24,
    GEO =='Alberta' ~ 48,
    GEO =='Prince Edward Island'  ~ 11,
    GEO =="British Columbia" ~ 59,
    GEO =="Saskatchewan" ~ 47,
    GEO =="Ontario" ~ 35,
    GEO =="Nova Scotia" ~ 12,
    GEO =="Manitoba" ~ 46
  ),
  exp_type = ifelse(exp_type == 'Clothing and footwear', 'Clothing', exp_type)) %>% 
  select(province, year, exp_type, price)

# Matched Dataset -----------------------------------------------------------------------------------------

# We need to make the matched dataset, which implies transforming from wide to long format

# Select some demographics

scs_to_match <-
  scs %>% 
  select(household, 
         year, 
         size, 
         province,
         rooms, 
         type, 
         tenure, 
         age_reference, 
         sex_reference,
         Food, 
         Shelter, 
         Transportation, 
         Clothing )

# Transform wide to long (which makes the dataset at the household-expenditure level, another type of panel)

scs_long_to_match <-
  scs_to_match %>% 
  gather(
    'exp_type',
    'expenditure',
    Food,
    Shelter,
    Transportation,
    Clothing
  )

# Join to the household dataset

matched_dataset <-
  scs_long_to_match %>% 
  left_join(cpi_to_match, by = c('year','province','exp_type'))

# Exporting -----------------------------------------------------------------------------------------------

scs_grouped %>%
  filter(province == 59) %>%
  kbl(caption = 'Mean Household Expenditure for British Columbia (rounded)',
      format = 'latex',
      digits = 0,
      booktabs = T)

matched_dataset %>% 
  slice_sample(n = 10) %>%
  kbl(caption = 'Matched Household Expenditure & Prices (rounded)',
      format = 'latex',
      digits = 0,
      booktabs = T)

  

