#packages
install.packages("AER")
install.packages("plm")
install.packages("tidyverse")
install.packages("stargazer")
install.packages("gtsummary")

library(AER)
library(plm)
library(tidyverse)
library(stargazer)
library(gtsummary)

# Import Dataset
ipums_mental_health <- read_csv("nhis_00007 (1).csv")

# Cleaning
# Create tibble from nhis_00007.csv
nhis <- ipums_mental_health


new_nhis <- nhis %>%
  filter(DEPFEELEVL < 7 & YEAR > 2018) %>%
  #select(YEAR, REGION, EMPSTAT, HOURSWRK, POVERTY, POVLEV, YDELAYMENTAL, DPCOUNSEL, WORRX, WORFEELEVL, DEPRX, DEPFEELEVL) %>%
  mutate(
    # Dummies for DEPFEELEVL
    no_dep = ifelse(DEPFEELEVL == 0, 1, 0),
    little_dep = ifelse(DEPFEELEVL == 1, 1, 0),
    lot_dep = ifelse(DEPFEELEVL == 2, 1, 0),
    some_bet = ifelse(DEPFEELEVL == 3, 1, 0),
    # Dummies for REGION
    northeast = ifelse(REGION == "1", 1, 0),
    midwest = ifelse(REGION == "2", 1, 0),
    south = ifelse(REGION == "3", 1, 0),
    west = ifelse(REGION == "4", 1, 0),
    # Dummies for SEX
    male = ifelse(SEX == 1, 1, 0),
    female = ifelse(SEX == 2, 1, 0),
    # Dummies for EDUC
    lessthanhs = ifelse(EDUC < 200, 1, 0),
    hsgrad = ifelse(EDUC >= 200 & EDUC <=202, 1, 0),
    somcollege = ifelse(EDUC >= 300 & EDUC <= 303, 1, 0),
    bachelors = ifelse(EDUC == 400, 1, 0),
    gradschool = ifelse(EDUC >= 500 & EDUC <=522, 1, 0),
    # Dummies for RACE
    white = ifelse(RACENEW == 100, 1, 0),
    black = ifelse(RACENEW == 200, 1, 0),
    nativeam = ifelse(RACENEW == 300, 1, 0),
    multipleraces = ifelse(RACENEW >= 500 & RACENEW <= 542, 1, 0),
    unknown = ifelse(RACENEW >= 997 & RACENEW <= 999, 1, 0),
    # Dummies for MARSTCUR
    married = ifelse(MARSTCUR >= 1 & MARSTCUR <= 3, 1, 0),
    separated = ifelse(MARSTCUR == 4, 1, 0),
    divorced = ifelse(MARSTCUR == 5, 1, 0),
    widowed = ifelse(MARSTCUR == 6, 1, 0),
    livingwithpart = ifelse(MARSTCUR == 7, 1, 0),
    nevermar = ifelse(MARSTCUR == 8, 1, 0),
    # Dummies for EMPSTAT
    employed = ifelse(EMPSTAT >= 100 & EMPSTAT <= 122, 1, 0),
    unemployed = ifelse(EMPSTAT >= 200 & EMPSTAT <= 220, 1, 0),
    # Age as quadratic
    agesq = AGE ^ 2,
    # Any depression dummy
    any_dep = ifelse(DEPFEELEVL >= 1, 1, 0),
    #Year dummies
    # YEAR fixed effects - hopefully dont have to actually use lol 
    yr19 = ifelse(YEAR == 2019, 1, 0),
    yr20 = ifelse(YEAR == 2020, 1, 0), 
    yr21 = ifelse(YEAR == 2021, 1, 0), 
    yr22 = ifelse(YEAR == 2022, 1, 0), 
    yr23 = ifelse(YEAR == 2023, 1, 0), 
    yr24 = ifelse(YEAR == 2024, 1, 0) 
  )



