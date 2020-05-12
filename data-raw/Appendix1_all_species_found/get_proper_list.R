# Script to generate table of all species found

library(STRIPS2veg)
library(dplyr)

data("veg_site")
data("extra_spp")
data("species_list")

all_spp <- veg_site %>%
  ungroup() %>%
  select(speciesID) %>%
  bind_rows(extra_spp %>% select(speciesID)) %>%
  distinct() %>%
  left_join(species_list)


