library(pacman)
p_load(rio,
       tidyverse,
       tidylog,
       ###
       ggalluvial,
       glue)

#import data
mort_data <- import("mortality_clean.rds")

total_cases <- n_distinct(mort_data$study_id)

mort_data |> 
  filter(elig_stat=="eligible") |> 
  filter(mort_stat=="assumed deceased") |> 
  count(study, ucod_leading, cod_multi) |> 
  arrange(n) |> 
  ggplot(aes(y=n,
             axis1=study, 
             axis4=ucod_leading, 
             axis5=cod_multi)) +
  geom_alluvium(aes(fill = study), show.legend = F) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_void()



