### -- Assignment 5a: Undirected Assignment -- ###

# Daniel Sánchez
# ECON836
# Due March 8th, 2023

# Preliminaries -----------------------------------------------------------

# Load libraries

library(tidyverse)
library(haven)

# Unzip files and load the data

# -- 2011

unzip('assignment5-undirected/data/ENEMDU_BDD_2011_12.zip', 
      exdir = 'assignment5-undirected/data')


unzip('assignment5-undirected/data/ENEMDU_PERSONAS_2011_12_hom.zip', 
      exdir = 'assignment5-undirected/data')

enemdu_2011_raw <-
  read_sav('assignment5-undirected/data/ENEMDU_PERSONAS_2011_12_hom.sav')

# I have to load with the SAV method since 

# -- 2012

unzip('assignment5-undirected/data/ENEMDU_BDD_2012_12.zip', 
      exdir = 'assignment5-undirected/data')

unzip('assignment5-undirected/data/ENEMDU_PERSONAS_2012_12_hom.zip', 
      exdir = 'assignment5-undirected/data')




