# combining and making table of extras
library(dplyr)

extras_2018 <- readr::read_csv("extraSpecies_2018.csv") %>%
  replace(is.na(.), 0) %>%
  tidyr::pivot_longer(cols = c(achmi:zizau), names_to = "speciesID") %>%
  arrange(siteID)%>%
  filter(value == 1)%>%
  mutate(year = "2018") %>%
  select(siteID, speciesID, year)

extras_2019 <- readr::read_csv("extraSpecies_2019.csv") %>%
  arrange(siteID) %>%
  mutate(year = "2019")

extra_spp <- bind_rows(extras_2018, extras_2019)%>%
  arrange(siteID)

usethis::use_data(extra_spp, overwrite = TRUE)
