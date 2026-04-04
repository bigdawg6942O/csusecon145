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
ipums_mental_health <- read_csv("nhis_00007.csv")

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
    any_dep = ifelse(DEPFEELEVL >= 1, 1, 0)
  )
# Descriptive Statistics and Visualization

# RACENEW to categorical variable
dem_nhis <- new_nhis %>%
  select(YEAR, RACENEW, DEPFEELEVL, POVLEV, SEX, AGE)
dem_nhis$RACENEW <- factor(dem_nhis$RACENEW,
                           levels = c(100, 200, 300, 400, 500, 510, 520, 530, 540, 541, 542, 997, 998, 999),
                           labels = c("White only", "Black/African American only", "American Indian", "Asian only", "Other & mult", "Othmult 2019", "Other", "Not Releasable", "Multiple Race", "Mult1999", "native and other", "unk refuse", "unk not asc", "unk idk")
                           )
dem_nhis$SEX <- factor(dem_nhis$SEX,
                       levels = c(1, 2, 7, 8, 9),
                       labels = c("Male", "Female", "Unk REF", "Unk NA", "Unk idk")
                       )
dem_nhis$catDEPFEELEVL <- factor(dem_nhis$DEPFEELEVL,
                                 levels = c(0, 1, 2, 3),
                                 labels = c("No depression", "little depression", "lot depression", "btwn little and lot")
)
#dem_nhis %>%
#select(RACENEW,catDEPFEELEVL,POVLEV,SEX,AGE) %>%
 # tbl_summary(
 #   statistic = list(all_categorical() ~ "{n} ({p})%"),
 #   AGE ~"{mean} ({sd})",
  #  POVLEV ~ "{mean} ({sd})",
  #  #digits = list(all_continuous() ~ c(2,2)
    # all_categorical() ~ c(0,1)),
   # type = list(
  #    RACENEW ~ "categorical",
  #    SEX ~ "categorical",
  #    catDEPFEELEVL ~ "categorical"
  #  ),
   # label = list(
    #  AGE ~ "Age",
    #  POVLEV ~ "% Over Poverty Line",
    #  RACENEW ~ "Race",
    #  SEX ~ "Gender",
    #  catDEPFEELEVL ~ "Levels of Depression",
   # )
#  )

# DEPFEELEVL to categorical

dem_nhis$RACENEW
summary(dem_nhis$RACE)

# Descriptive Statistics 


descriptive_stat <- as.data.frame(new_nhis) %>%
  select(no_dep, little_dep, lot_dep, some_bet, northeast, midwest, south, west, male, female, lessthanhs, hsgrad, somcollege, bachelors, gradschool, white, black, nativeam, multipleraces, unknown, married, separated, divorced, widowed, livingwithpart, nevermar, unemployed, AGE)
stargazer(descriptive_stat, 
          type = "text", 
          omit.summary.stat = c("min", "max", "N"), 
          title = "Descriptive Statistics",
          covariate.labels = c("No Depression", "Little Depression", "Lot of Depression", "Medium Depression", "Northeast", "Midwest", "South", "West", "Male", "Female", "Less than HS", "HS Graduate", "Some College", "Bachelors", "Grad School", "White", "Black", "Native American", "Multiple Races", "Unknown", "Married", "Separated", "Divorced", "Widowed", "Living with Partner", "Never Married", "Unemployed", "Age"),
          out = "descriptive_stats.txt"
          )

# Demographics
desc_dem <- dem_nhis %>%
  group_by(RACENEW) %>%
  summarise(
    count = n(),
    #pct_depressed = sum(DEPFEELEVL > 0) / n(),
    pct_depressed0 = sum(DEPFEELEVL == 0) / n(),
    pct_depressed1 = sum(DEPFEELEVL == 1) / n(),
    pct_depressed2 = sum(DEPFEELEVL == 2) / n(),
    pct_depressed3 = sum(DEPFEELEVL == 3) / n(),
    income = mean(POVLEV),
    sd_income = sd(POVLEV)
  )

desc_sex <- dem_nhis %>%
  group_by(SEX) %>%
  summarise(
    count = n(),
    pct_depressed0 = sum(DEPFEELEVL == 0) / n(),
    pct_depressed1 = sum(DEPFEELEVL == 1) / n(),
    pct_depressed2 = sum(DEPFEELEVL == 2) / n(),
    pct_depressed3 = sum(DEPFEELEVL == 3) / n(),
    income = mean(POVLEV),
    sd_income = sd(POVLEV)
  )

desc_age <- dem_nhis %>%
  #filter(AGE < 900)%>% 
  mutate(
    age_group = case_when(
      AGE >= 0 & AGE <= 10 ~ "0-10",
      AGE >10 & AGE <= 20 ~ "10-20",
      AGE > 20 & AGE <= 30 ~ "20-30",
      AGE > 30 & AGE <= 40 ~ "30-40",
      AGE > 40 & AGE <= 50 ~ "40-50",
      AGE > 50 & AGE <= 60 ~ "50-60",
      AGE > 60 & AGE <= 70 ~ "60-70",
      AGE > 70 & AGE <= 80 ~ "70-80",
      AGE > 80 & AGE < 85 ~ "80-85",
      AGE == 085 ~ "85+",
      AGE == 997 ~ "UNK - REF",
      AGE == 998 ~ "UNK - NA",
      AGE == 999 ~ "UNK - idk"
    ),
    age_group = factor(
      age_group,
      level = c("0-10","10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-85", "85+", "UNK - REF", "UNK - NA", "UNK - idk")
    )
  ) %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    pct_depressed0 = sum(DEPFEELEVL == 0) / n(),
    pct_depressed1 = sum(DEPFEELEVL == 1) / n(),
    pct_depressed2 = sum(DEPFEELEVL == 2) / n(),
    pct_depressed3 = sum(DEPFEELEVL == 3) / n(),
    income = mean(POVLEV),
    sd_income = sd(POVLEV)
  )


# Depression on Poverty Level Box Plot
visdep_nhis <- new_nhis %>%
  select(YEAR, DEPFEELEVL, POVLEV, AGE, REGION, SEX)

#DEPFEELEVL to categorical
visdep_nhis$DEPFEELEVL <- factor(dem_nhis$DEPFEELEVL,
  levels = c(0, 1, 2, 3),
   labels = c("No Depression", "Little Depression", "Lot Depression", "Btwn Little and Lot")
  )


visdep_nhis$DEPFEELEVL <- factor(visdep_nhis$DEPFEELEVL, levels = c("No Depression", "Little Depression", "Btwn Little and Lot", "Lot Depression"))

# Depression Histogram
ggplot(visdep_nhis, aes(x = DEPFEELEVL)) +
  geom_bar() +
  labs(
    title = "Overall Distribution of Depression Symptoms",
    x = "Level of Depression",
    y = "Count"
  ) +
  geom_text(
    stat = "count", aes(label = ..count..), vjust =-1
  )

# Descriptive Statistics by Level of Depression

desc_dep <- visdep_nhis %>%
  group_by(DEPFEELEVL) %>%
  summarise(
    count = n(),
    avg_povlev = mean(POVLEV),
    sd_povlev = sd(POVLEV),
    avg_age = mean(AGE),
    sd_age = sd(AGE)
  )



ggplot(visdep_nhis, mapping = aes(x = DEPFEELEVL, y = POVLEV)) +
  geom_boxplot() + 
  labs(
    title = "Depression's Effect on Family Income as a % of Poverty Line",
    x = "Level of Depression",
    y = "Family Income as % of Poverty Level"
  ) 

# Descriptive Statistics for Instruments
#Adltputdown only available from '21 - '23

desc_inst <- new_nhis %>%
  filter(YEAR >= 2021 & YEAR <= 2023) %>%
  select(YDELAYMENTAL, VIOLENEV, MENTDEPEV, ADLTPUTDOWN, POVLEV, DEPFEELEVL) %>%
  pivot_longer(
    cols = c("YDELAYMENTAL", "VIOLENEV", "MENTDEPEV", "ADLTPUTDOWN"),
    names_to = "instrument",
    values_to = "response"
  ) %>%
  filter(
    response <3 & response > 0
  ) %>%
  group_by(instrument, response) %>%
  summarise(
    count = n()
  )



# Models

# lm() with FE dummies
model1 <- lm(POVLEV ~ little_dep + lot_dep + some_bet + northeast + midwest + south + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed + YEAR, data = new_nhis)
# Reference points for dummies: REGION - west, SEX - female, EDUC - less than hs, RACE - white, MARSTCUR - never married, EMPSTAT - unemployed
stargazer(model1, type = "text", title = "Depression's Impact on % Above the Poverty Level [model1]")

# plm() two way FE
model2 <- plm(POVLEV ~ little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
              data = new_nhis,
              index = c("REGION", "YEAR"),
              model = "within",
              effect = "twoways")

stargazer(model1, type = "text", title = "Depression's Impact on % Above the Poverty Level [model2]")

# TSLS, using YDELAYMENTAL as an instrument, any_dep as dependent variable

# First Stage
anydeprobit <- glm(any_dep ~ YDELAYMENTAL,
                   family = binomial(link = "probit"),
                   data = new_nhis)
stargazer(anydeprobit, type = "text")
anydepredict <- anydeprobit$fitted.values
# Instrument Relevance
linearHypothesis(anydeprobit,
                 "YDELAYMENTAL = 0",
                 vcov = vcovHC, type = "HC1")
# Second Stage
tslsmodel <- plm(POVLEV ~ anydepredict + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                 data = new_nhis,
                 index = c("REGION", "YEAR"),
                 model = "within",
                 effect = "twoways")

olsmodel <- plm(POVLEV ~ any_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                data = new_nhis,
                index = c("REGION", "YEAR"),
                model = "within",
                effect = "twoways")

stargazer(tslsmodel, olsmodel, type = "text", title = "TSLS vs OLS", object.names = TRUE, object.numbers = FALSE)
