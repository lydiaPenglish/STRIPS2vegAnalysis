# Format vegetation.rda file in a wide format (veg_mid.rda) and into files with diversity
# and richness

library(STRIPS2veg)       # data
data("vegetation")
data("species_list")
data("all_site_info")
library(dplyr)

midpoints <- 
  vegetation %>%
  mutate(midpoint = recode(cover, 
                           '<1' = 0.5, 
                           '1-5' = 3 ,
                           '5-25' = 15, 
                           '25-50' = 37.5,
                           '50-75' = 62.5, 
                           '75-95' = 85, 
                           '>95' = 97.5))%>%
  select(year:speciesID, midpoint) %>%
  filter(!stringr::str_detect(quadratID, 
                              "bue_2_\\d|isb_2_\\d|stt_[123]_\\d|gos_[^123]_\\d"))

# calculating avg area:perimeter ratio of strips
data("strips")

pa_rat <- strips %>%
  mutate(perim_area = perimeter/area) %>%
  group_by(siteID) %>%
  summarize(n_strips = n(),
            avg_p_a  = mean(perim_area))

all_site_info <- left_join(all_site_info, pa_rat)

# wide dataframe
veg_mid <- 
  midpoints %>%
  tidyr::pivot_wider(id_cols = c(year, quadratID, siteID), names_from = speciesID, 
                     values_from = midpoint) %>%
  arrange(year, siteID) %>%
  replace(., is.na(.), 0) %>%
  select(year, quadratID, siteID, sort(tidyselect::peek_vars()))

usethis::use_data(veg_mid, overwrite = TRUE)

# long dataframe 
veg_site <- 
  midpoints %>%
  group_by(year, siteID, speciesID) %>%
  summarize(cov = sum(midpoint)) %>%
  group_by(year, siteID) %>%
  mutate(total_cover = sum(cov),
         pi          = cov/total_cover) %>%
  left_join(species_list)

usethis::use_data(veg_site, overwrite = TRUE)

# gamma, alpha, and beta diversity 
gamma_div <- 
  veg_mid %>%
  select(year, siteID, abuth:zizau) %>%
  group_by(year, siteID) %>%
  summarize_all(sum) %>%
  ungroup() %>%
  mutate(gamma_div = exp(vegan::diversity(.[, 3:ncol(.)]))) %>%
  select(year, siteID, gamma_div)

site_quad_year <- 
  veg_mid %>%
  select(year, siteID, quadratID) 
alpha_nested <- 
  veg_mid %>%
  tibble::remove_rownames() %>%
  group_by(year, siteID)%>%
  tidyr::nest() %>%
  mutate(alpha_div = purrr::map(data, ~exp(vegan::diversity(.[, 2:ncol(.)]))))
alpha_div <- 
  alpha_nested$alpha_div %>%
  purrr::map(., as.data.frame)%>%
  bind_rows()%>%
  bind_cols(site_quad_year)%>%
  rename("alpha_div" = ".x[[i]]")

beta_div <- 
  alpha_div %>%
  group_by(year, siteID) %>%
  mutate(avg_alpha = mean(alpha_div)) %>%
  ungroup() %>%
  select(-c(quadratID, alpha_div)) %>%
  distinct() %>%
  left_join(gamma_div, by = c("year", "siteID")) %>%
  mutate(beta_div = gamma_div/avg_alpha)

# prairie and weedy species richness at site and quadrat scale
nat_codes <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^prairie")) %>%
  select(speciesID) %>%
  unlist()
weeds <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy")) %>%
  select(speciesID) %>%
  unlist()

site_rich <- 
  veg_mid %>%
  group_by(year, siteID) %>%
  summarize_if(is.numeric, sum) %>%
  mutate_at(vars(abuth:zizau), ~1* (. > 0)) %>%
  ungroup()%>%
  mutate(rich = rowSums(.[3:ncol(.)])) %>%
  select(year, siteID, rich)

site_p_rich <- 
  veg_mid %>%
  select(year, siteID, one_of(nat_codes)) %>%
  group_by(year, siteID) %>%
  summarize_all(sum) %>%
  mutate_at(vars(achmi:zizau), ~1* (. > 0)) %>%
  ungroup() %>%
  mutate(p_rich = rowSums(.[3:ncol(.)])) %>%
  select(year, siteID, p_rich)

site_w_rich <- 
  veg_mid %>%
  select(year, siteID, one_of(weeds)) %>%
  group_by(year, siteID) %>%
  summarize_all(sum) %>%
  mutate_at(vars(abuth:zeama), ~1* (. > 0)) %>%
  ungroup() %>%
  mutate(w_rich = rowSums(.[3:ncol(.)])) %>%
  select(year, siteID, w_rich)

quad_p_rich <- 
  veg_mid %>%
  select(year, siteID, quadratID, one_of(nat_codes)) %>%
  mutate_at(vars(achmi:zizau), ~1* (. > 0)) %>%
  mutate(p_rich = rowSums(.[4:ncol(.)])) %>%
  select(year, siteID, quadratID, p_rich)

quad_w_rich <- 
  veg_mid %>%
  select(year, siteID, quadratID, one_of(weeds)) %>%
  mutate_at(vars(abuth:zeama), ~1* (. > 0)) %>%
  mutate(w_rich = rowSums(.[4:ncol(.)])) %>%
  select(year, siteID, quadratID, w_rich)

site_div_rich <- 
  beta_div %>%
  left_join(site_rich) %>%
  left_join(site_p_rich) %>%
  left_join(site_w_rich) %>%
  left_join(all_site_info)

usethis::use_data(site_div_rich, overwrite = TRUE)

quad_div_rich <- 
  alpha_div %>%
  left_join(quad_p_rich) %>%
  left_join(quad_w_rich) %>%
  left_join(all_site_info)

usethis::use_data(quad_div_rich, overwrite = TRUE)
