# This script make a file that has the relative and avg cover of different functional groups

library(dplyr)
library(STRIPS2veg)
data("species_list")

# - groups of codes for different functional groups - #
nat_grass <- species_list %>%
  filter(group == "prairie C4 grass"|group == "prairie C3 grass"|group == "prairie sedge")%>%
  select(speciesID)%>%
  unlist()
nat_forbs <- species_list %>%
  filter(group == "prairie forb")%>%
  select(speciesID)%>%
  unlist()
weed_annuals <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy"), life_cycle == "annual") %>%
  select(speciesID) %>%
  unlist()
weed_perenn <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy"), life_cycle == "perennial" | life_cycle == "biennial") %>%
  select(speciesID) %>%
  unlist()
nat_codes <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^prairie")) %>%
  select(speciesID) %>%
  unlist()
weeds <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy")) %>%
  select(speciesID) %>%
  unlist()

data("veg_mid")
data("veg_site")

# average cover of each functional group 

get_cov <- function(group, data = veg_mid){
  dd <- data %>%
    dplyr::select(year, siteID, quadratID, one_of(group)) %>%
    mutate(cov = rowSums(.[, 4:ncol(.)])) %>%
    group_by(siteID, year) %>%
    summarize(avg_cov = mean(cov),
              sd_cov  = sd(cov),
              n_cov   = n(),
              se_cov  = sd_cov/sqrt(n_cov)) %>%
    dplyr::select(year, siteID, avg_cov, sd_cov, n_cov, se_cov) 
  return(dd)
}

grass_cov <- get_cov(group = nat_grass) %>% left_join(all_site_info)
forb_cov  <- get_cov(group = nat_forbs) %>% left_join(all_site_info)
nat_cov   <- get_cov(group = nat_codes) %>% left_join(all_site_info)
pw_cov    <- get_cov(group = weed_perenn) %>% left_join(all_site_info)
aw_cov    <- get_cov(group = weed_annuals) %>% left_join(all_site_info)
weed_cov  <- get_cov(group = weeds) %>% left_join(all_site_info)

# relative cover of each functional group
prairie_pi <- 
  left_join(
    veg_site %>%
    filter(speciesID %in% nat_grass) %>%
    group_by(year, siteID, total_cover) %>%
    summarize(pg_cov = sum(cov)) %>%
    mutate(pg_pi = pg_cov/total_cover)
  ,
    veg_site %>%
    filter(speciesID %in% nat_forbs) %>%
    group_by(year, siteID, total_cover) %>%
    summarize(pf_cov = sum(cov)) %>%
    mutate(pf_pi = pf_cov/total_cover)
) %>%
  mutate(prairie_cov = pg_cov + pf_cov,
         prairie_pi  = prairie_cov/total_cover) %>%
  left_join(all_site_info)

weedy_pi <- 
  left_join(
    veg_site %>%
    filter(speciesID %in% weed_annuals) %>%
    group_by(year, siteID, total_cover) %>%
    summarize(wa_cov = sum(cov)) %>%
    mutate(wa_pi = wa_cov/total_cover)
  ,
    veg_site %>%
    filter(speciesID %in% weed_perenn) %>%
    group_by(year, siteID, total_cover) %>%
    summarize(wp_cov = sum(cov)) %>%
    mutate(wp_pi = wp_cov/total_cover)
) %>%
  mutate(weed_cov = wa_cov + wp_cov,
         weed_pi  = weed_cov/total_cover) %>%
  left_join(all_site_info)

