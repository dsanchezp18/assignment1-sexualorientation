# Midterm coding practice

# Load libraries ------------------------------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(WDI)

# Load world bank data ------------------------------------------------------------------------------------

indicators <- c(
  
  'inv_gdp'= 'NE.GDI.FTOT.ZS', #  Gross fixed capital formation (% of GDP)
  'gdp_pc' = 'NY.GDP.PCAP.KD',
  'gdp_g'= 'NY.GDP.PCAP.KD.ZG', # GDP per capita growth (annual %)
  'enrolment' = 'SE.PRM.ENRR' # Gross primary school enrolment
)

# Now get my data

wdi_raw <-
  WDI(indicator = indicators,  
      extra = T)


# Data Cleaning -------------------------------------------------------------------------------------------

# Remove aggregates

df <-
  wdi_raw %>% 
  filter(region != 'Aggregates')

# Table of means ------------------------------------------------------------------------------------------

stargazer(summary = T)