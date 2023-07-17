# Midterm coding practice

# Load libraries ------------------------------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(AER)
library(fixest)

# Create some fake data ---------------------------------------------------

# Now get my data
df <- data.frame(y = runif(100,1,20),
                 x1 = runif(100, 1, 10),
                 x2 = runif(100, 5, 15),
                 x3 = runif(100, 10, 20),
                 x4 = runif(100, 15, 25),
                 x5 = runif(100, 20, 30),
                 x6 = runif(100, 25, 35),
                 x7 = runif(100, 30, 40),
                 x8 = runif(100, 35, 45),
                 x9 = runif(100, 40, 50),
                 x10 = runif(100, 45, 55))


# Table of means ----------------------------------------------------------

df %>% 
  summarise(across(x1:x10, mean))

# IV regression -----------------------------------------------------------

# Load the "CollegeDistance" dataset

data("CollegeDistance")

# Fit an IV regression

iv_model <- ivreg(log(wage) ~  gender + score | distance + score,
                  data = CollegeDistance)

summary(iv_model)

# View the results

summary(iv_model)

# Another intent by trying to do what he asks you to

iv_model2 <- ivreg(y ~ . - x9 - x10 | . - x1 , 
                   data = df)

summary(iv_model2)

# With feols

iv_model3 <- 
  feols(y ~ x2 + x3 +x4 + x5 + x6 + x7 + x8 | x1 ~ x9 + x10,
        data = df)

summary(iv_model3)

iv_model3$iv_first_stage

