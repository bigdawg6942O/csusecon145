
# OLS //////////////////////////

olsmodeltwoway <- plm(POVLEV ~ little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                      data = new_nhis,
                      index = c("REGION", "YEAR"),
                      model = "within",
                      effect = "twoways")
olsmodelpooled <- plm(POVLEV ~ little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                      data = new_nhis,
                      model = "pooling")
olsmodeltime <- plm(POVLEV ~ little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                    data = new_nhis,
                    index = c("YEAR"),
                    model = "within",
                    effect = "time")
olsmodelregion <- plm(POVLEV ~ little_dep + lot_dep + some_bet + AGE + agesq + male + hsgrad + somcollege + bachelors + gradschool + black + nativeam + multipleraces + married + separated + divorced + widowed + livingwithpart + employed, 
                      data = new_nhis,
                      index = c("REGION"),
                      model = "within",
                      effect = "individual")


stargazer(olsmodeltwoway, olsmodelpooled, olsmodeltime, olsmodelregion, type = "text", column.labels = c("Two Way FE","Pooled FE", "Year FE", "Region FE"), title = "OLS Fixed Effects Results: Level of Depression on Multiple of the Poverty Level")

stargazer(olsmodelpooled, type = "text", title = "Pooled OLS")