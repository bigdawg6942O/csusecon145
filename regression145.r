#REGRESSION ///////////////////
# lm() with FE dummies
model1 <- lm(POVLEV ~ 
               # Variables
               little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed + 
               # Brute Force Region Fixed Effects // exclude west
               northeast + midwest + south +
               # Brute Force Time Fixed Effects // exclude yr19
               yr20 + yr21 + yr22 + yr23 + yr24, 
             data = new_nhis)
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

# First Stage /////////////////////////////
anydeprobit <- glm(any_dep ~ YDELAYMENTAL+
                     #variables
                     AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed + 
                     # Brute Force Region Fixed Effects // exclude west
                     northeast + midwest + south +
                     # Brute Force Time Fixed Effects // exclude yr19
                     yr20 + yr21 + yr22 + yr23 + yr24, 
                   family = binomial(link = "probit"),
                   data = new_nhis)
stargazer(anydeprobit, type = "text")
anydepredict <- anydeprobit$fitted.values
# Instrument Relevance
linearHypothesis(anydeprobit,
                 "YDELAYMENTAL = 0",
                 vcov = vcovHC, type = "HC1")
# Second Stage /////////////////////////////

tslsmodellm <- lm(POVLEV ~ anydepredict +
                    AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed + 
                    # Brute Force Region Fixed Effects // exclude west
                    northeast + midwest + south +
                    # Brute Force Time Fixed Effects // exclude yr19
                    yr20 + yr21 + yr22 + yr23 + yr24, 
                  data= new_nhis
)
stargazer(tslsmodellm, type = "text")


tslsmodel <- plm(POVLEV ~ anydepredict + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                 data = new_nhis,
                 index = c("REGION", "YEAR"),
                 model = "within",
                 effect = "twoways")

# OLS Fixed Effects //////////////////////////

olsmodeltwoway <- plm(POVLEV ~ little_dep + some_bet + lot_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                data = new_nhis,
                index = c("REGION", "YEAR"),
                model = "within",
                effect = "twoways")
olsmodelpooled <- plm(POVLEV ~ little_dep + some_bet + lot_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                      data = new_nhis,
                      model = "pooling")
olsmodeltime <- plm(POVLEV ~ little_dep + some_bet + lot_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                                      data = new_nhis,
                                      index = c("YEAR"),
                                      model = "within",
                                      effect = "time")
olsmodelregion <- plm(POVLEV ~ little_dep + some_bet + lot_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                    data = new_nhis,
                    index = c("REGION"),
                    model = "within",
                    effect = "individual")



# OLS Random Effects /////////////////////////
olsmodelrandom <- plm(POVLEV ~ little_dep + some_bet + lot_dep + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                      data = new_nhis,
                      index = c("REGION", "YEAR"),
                      model = "random",
                      random.method = "amemiya",
                      effect = "twoways") 
stargazer(olsmodelrandom, type = "text", keep = c("little_dep", "some_bet", "lot_dep"))

# Regression Output ////////////////////////////
stargazer(olsmodeltwoway, olsmodeltime, olsmodelregion, olsmodelrandom, 
          type = "text", 
          column.labels = c("Two Way FE", "Year FE", "Region FE","Random Effects"),# "Pooled OLS"), 
          dep.var.labels = "Multiple of Poverty Level",
          covariate.labels = c("Little Depression", "Medium Depression", "Lot Depression", "Age", "Age Squared", "Male", "HS Grad", "Some College", "Bachelors", "Grad School", "Black", "Native American", "Multiple Races", "Married", "Separated", "Divorced", "
                               Widowed", "Living with Partner", "Employed"),
          title = "Table 4.2: Fixed and Random Effects OLS Results: Level of Depression on Multiple of the Poverty Level", 
          out = "regression.text")
stargazer(olsmodelpooled, 
          type = "text", 
          column.labels = "Pooled OLS", 
          dep.var.labels = "Multiple of Poverty Level",
          covariate.labels = c("Little Depression", "Medium Depression", "Lot Depression", "Age", "Age Squared", "Male", "HS Grad", "Some College", "Bachelors", "Grad School", "Black", "Native American", "Multiple Races", "Married", "Separated", "Divorced", "
                               Widowed", "Living with Partner", "Employed"),
          title = "Table 4.1: Pooled OLS Results: Level of Depression on Multiple of the Poverty Level", 
          out = "regression.text")
# Presentation Regression Output
stargazer(olsmodeltwoway, olsmodeltime, olsmodelregion, olsmodelrandom, 
          type = "text", 
          keep = c("little_dep", "some_bet", "lot_dep"),
          column.labels = c("Two Way FE", "Year FE", "Region FE","Random Effects"),# "Pooled OLS"), 
          dep.var.labels = "Multiple of Poverty Level",
          covariate.labels = c("Little Depression", "Medium Depression", "Lot Depression", "Age", "Age Squared", "Male", "HS Grad", "Some College", "Bachelors", "Grad School", "Black", "Native American", "Multiple Races", "Married", "Separated", "Divorced", "
                               Widowed", "Living with Partner", "Employed"),
          title = "Table 4.2: Fixed and Random Effects OLS Results: Level of Depression on Multiple of the Poverty Level", 
          out = "regression.text")

stargazer(olsmodelpooled, 
          type = "text", 
          keep = c("little_dep", "some_bet", "lot_dep"),
          column.labels = "Pooled OLS", 
          dep.var.labels = "Multiple of Poverty Level",
          covariate.labels = c("Little Depression", "Medium Depression", "Lot Depression", "Age", "Age Squared", "Male", "HS Grad", "Some College", "Bachelors", "Grad School", "Black", "Native American", "Multiple Races", "Married", "Separated", "Divorced", "
                               Widowed", "Living with Partner", "Employed"),
          title = "Table 4.1: Pooled OLS Results: Level of Depression on Multiple of the Poverty Level", 
          out = "regression.text")

# Specification Tests //////////////////

# Hausman Test
phtest(olsmodeltwoway, olsmodelrandom)

# Breusch-Pagan Test
bptest(olsmodelpooled)


