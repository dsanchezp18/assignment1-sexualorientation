
library(tidyverse) # For data wrangling
library(haven) # Reading stata files
library(labelled) # For survey labels
library(modelsummary) # For tables
library(data.table) # For combinations

df<-
  read_dta('data/Census_2016_Hierarchial.dta')