# Instruments 

violence_table <- new_nhis %>% 
  filter( YEAR >= 2021 & YEAR <= 2023) %>%
  select(YEAR, POVLEV, DEPFEELEVL, VIOLENEV)%>%
  group_by(VIOLENEV, DEPFEELEVL) %>%
  summarise(
    n()
  )
