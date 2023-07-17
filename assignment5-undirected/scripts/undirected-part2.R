### -- Assignment 5a: Undirected Assignment -- ###

# Daniel Sánchez
# ECON836
# Due March 8th, 2023

# Preliminaries -----------------------------------------------------------

# Load libraries

library(tidyverse)
library(haven)
library(lubridate)
library(fixest)
library(oaxaca)
library(quantreg)

# Unzip files and load the data

# -- 2011

unzip('assignment5-undirected/data/ENEMDU_BDD_2011_12.zip', 
      exdir = 'assignment5-undirected/data')


unzip('assignment5-undirected/data/ENEMDU_PERSONAS_2011_12_hom.zip', 
      exdir = 'assignment5-undirected/data')

enemdu_2011_raw <-
  read_sav('assignment5-undirected/data/ENEMDU_PERSONAS_2011_12_hom.sav')

# I have to load with the SAV method since it includes all variable and value labels.

# -- 2012

unzip('assignment5-undirected/data/ENEMDU_BDD_2012_12.zip', 
      exdir = 'assignment5-undirected/data')

unzip('assignment5-undirected/data/ENEMDU_PERSONAS_2012_12_hom.zip', 
      exdir = 'assignment5-undirected/data')

enemdu_2012_raw <-
  read_sav('assignment5-undirected/data/ENEMDU_PERSONAS_2012_12_hom.sav')

# Data Preparation --------------------------------------------------------

# I prepare the data below for the econometric analysis.

# Bind these two together, and start a **data cleaning pipeline**

df <-
  enemdu_2011_raw %>% 
  bind_rows(enemdu_2012_raw) %>% 
  mutate(period = parse_date(as.character(periodo), format = '%Y%m'),
         year = year(period) %>% as.factor()) %>% 
  rename(ppid = 'id_persona',
         hhid = 'id_hogar',
         school_status = 'p10a',
         school_year = 'p10b',
         employed = 'empleo',
         type = 'rama1') %>% 
  mutate(sex = if_else(p02 == 1, 'Male', 'Female') %>% as.factor() %>% relevel(ref = 'Male'),
         income = na_if(ingrl, 999999) %>% zap_labels() %>% unname(),
         age = na_if(p03, 99) %>% zap_labels() %>% unname(),
         mar_stat = case_when(
           p06 == 1 ~ 'Married',
           p06 == 2 ~ 'Separated',
           p06 == 3 ~ 'Divorced',
           p06 == 4 ~ 'Widowed',
           p06 == 5 ~ 'Common Law',
           p06 == 6 ~ 'Single',
         ) %>% as.factor() %>% relevel(ref = 'Single'),
         race = case_when(
           p15 == 1 ~ 'Indigenous',
           p15 %in% c(2,3) ~ 'Afroecuadorian/Black',
           p15 == 4 ~ 'Mulatto',
           p15 == 5 ~ 'Montubio',
           p15 == 6 ~ 'Mestizo',
           p15 == 7 ~ 'White',
           p15 == 8 ~ 'Other'
         ) %>% as.factor() %>% relevel(ref = 'Mestizo'),
         hh_head = if_else(p04 == 1, 'Head', 'Not Head') %>% as.factor() %>% relevel(ref = 'Not Head'),
         urban = if_else(area == 1, 'Urban', 'Rural') %>% as.factor() %>% relevel(ref = 'Rural'),
         public = if_else(p42 == 1, 'Public', 'Not Public') %>% as.factor() %>% relevel(ref =  'Not Public'),
         exp = na_if(p45, 99) %>% zap_labels() %>% unname(),
         schooling = case_when(
           nnivins == 1 ~ 'None',
           nnivins == 2 ~ 'Alphabetization Center',
           nnivins == 3  ~ 'Basic',
           nnivins == 4 ~ 'Secondary',
           nnivins == 5  ~ 'Higher',
         ) %>% as.factor() %>% relevel(ref = 'None'),
         sex_dummy = as.logical(sex == 'Female'),
         hours = if_else(pp02g == 999, NA, pp02g*4)) %>%
  filter(income > 0,
         employed == 1,
         hours > 0) %>% 
  mutate(income_h = income/hours)

# Run models --------------------------------------------------------------

# 2011, everyone

m11_all <- lm(
  log(income) ~ sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2011),
  weights = fexp
)

summary(m11_all)

# 2011, women

m11_women <- lm(
  log(income) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2011, sex == 'Female'),
  weights = fexp
)

summary(m11_women)

# 2011, men

m11_men <- lm(
  log(income) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2) ,
  data = df %>% filter(year == 2011, sex == 'Male'),
  weights = fexp
)

summary(m11_men)


# 2012, everyone

m12_all <- lm(
  log(income) ~ sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012),
  weights = fexp
)

summary(m12_all)

# 2012, women

m11_women <- lm(
  log(income) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012, sex == 'Female'),
  weights = fexp
)

summary(m11_women)

# 2012, men

m11_men <- lm(
  log(income) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012, sex == 'Male'),
  weights = fexp
)

summary(m11_men)

# Both years for all

m_all <- lm(
  log(income) ~ year + sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp
)

summary(m_all)

# Interact year with sex to see the change in the gender differential

m_all_int <- lm(
  log(income) ~ year*sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp
)

summary(m_all_int)

# Regressions with earnings per hour --------------------------------------

# 2011, everyone

m11_allh <- lm(
  log(income_h) ~ sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2011),
  weights = fexp
)

summary(m11_all)

# 2011, women

m11_womenh <- lm(
  log(income_h) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2011, sex == 'Female'),
  weights = fexp
)

summary(m11_women)

# 2011, men

m11_menh <- lm(
  log(income_h) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2011, sex == 'Male'),
  weights = fexp
)

summary(m11_men)

# 2012, everyone

m12_allh <- lm(
  log(income_h) ~ sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012),
  weights = fexp
)

summary(m12_all)

# 2012, women

m11_womenh <- lm(
  log(income_h) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012, sex == 'Female'),
  weights = fexp
)

summary(m11_women)

# 2012, men

m11_menh <- lm(
  log(income_h) ~ age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(year == 2012, sex == 'Male'),
  weights = fexp
)

summary(m11_men)

# Both years for all

m_allh <- lm(
  log(income_h) ~ year + sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp
)

summary(m_allh)

# Both years for men

m_allh_men <- lm(
  log(income_h) ~ year + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df %>% filter(sex == 'Male'),
  weights = fexp
)

summary(m_allh_men)

# Interact year with sex to see the change in the gender differential

m_all_inth <- lm(
  log(income_h) ~ year*sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp
)

summary(m_all_int)

# Quantile Regressions ----------------------------------------------------

quant_allh <- rq(
  log(income_h) ~ year*sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp,
  tau = c(0.1,0.9)
)

quant_all <-rq(
  log(income) ~ year*sex + age + schooling + mar_stat + race + urban + public + exp + I(exp^2),
  data = df,
  weights = fexp,
  tau = c(0.1,0.9)
)

# Oaxaca-Blinder Decompositions -------------------------------------------

# For both years and total income

oax <-
  oaxaca(log(income) ~ year + age + schooling + mar_stat + race + urban + public + exp + I(exp^2) | sex_dummy,
         df)

# For both years with hourly income

oax_h <-
  oaxaca(log(income_h) ~ year + age + schooling + mar_stat + race + urban + public + exp + I(exp^2) | sex_dummy,
         df)

# Finish, exporting everything to an .RData file to later load to my Quarto document.

save(list = ls(), file = 'assignment5-undirected/report/environment.RData')

