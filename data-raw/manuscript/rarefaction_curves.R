# Making rarefaction curves for each site to show how sufficient sampling effort was

library(dplyr)
library(STRIPS2veg)
library(iNEXT)
library(ggplot2)

data("veg_mid")

# Start with 2018
veg_18 <- veg_mid %>%
  filter(year == "2018") %>%
  select(-year) %>%
# making dataframe into new format  
  tidyr::pivot_longer(cols = abuth:zizau, names_to = "species", values_to = "abund") %>%
  tidyr::pivot_wider(id_cols = c(siteID, species), names_from = quadratID, values_from = abund) %>%
# incidence only   
  mutate(across(where(is.numeric), ~1 * (. > 0)),
         siteID = stringr::str_to_lower(siteID)) 

sites_18 <- veg_18 %>% distinct(siteID) %>% unlist() %>% unname()

list_18_names <- sites_18
list_18 <- vector("list", length(list_18_names))
names(list_18) <- list_18_names 

for (i in seq_along(sites_18)) {            
  list_18[[i]] <- veg_18 %>%
    filter(siteID == sites_18[[i]]) %>%
             select(species, starts_with(sites_18[[i]])) %>%
    tibble::column_to_rownames("species") %>%
    as.incfreq(.)
}

isites_18 <- iNEXT(list_18, q=0, datatype = "incidence_freq")
ggiNEXT(isites_18, facet.var = "site")+
  labs(x = "Number of Quadrats",
       y = "Species Richness",
       title = "2018 sites rarefaction curves")+
  facet_wrap(~site, nrow = 5)+
  guides(lty = FALSE, color = FALSE, shape = FALSE, fill = FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "white"))

# 2019 data 

veg_19 <- veg_mid %>%
  filter(year == "2019") %>%
  select(-year) %>%
  # making dataframe into new format  
  tidyr::pivot_longer(cols = abuth:zizau, names_to = "species", values_to = "abund") %>%
  tidyr::pivot_wider(id_cols = c(siteID, species), names_from = quadratID, values_from = abund) %>%
  # incidence only   
  mutate(across(where(is.numeric), ~1 * (. > 0)),
         siteID = stringr::str_to_lower(siteID)) 

sites_19 <- veg_19 %>% distinct(siteID) %>% unlist() %>% unname()

list_19_names <- sites_19
list_19 <- vector("list", length(list_19_names))
names(list_19) <- list_19_names 

for (i in seq_along(sites_19)) {            
  list_19[[i]] <- veg_19 %>%
    filter(siteID == sites_19[[i]]) %>%
    select(species, starts_with(sites_19[[i]])) %>%
    tibble::column_to_rownames("species") %>%
    as.incfreq(.)
}

isites_19 <- iNEXT(list_19, q=0, datatype = "incidence_freq")
plots_19 <- ggiNEXT(isites_19, facet.var = "site")+
  labs(x = "Number of Quadrats",
       y = "Species Richness",
       title = "2019 sites rarefaction curves")+
  facet_wrap(~site, nrow = 5)+
  guides(lty = FALSE, color = FALSE, shape = FALSE, fill = FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "white"))
ggsave("rarefaction_2019.png", plots_19, dpi = 500, width = 12, height = 9)

## making up 2019 curves for prairie species only

data("species_list")
pra_codes <- species_list %>%
  filter(stringr::str_detect(group, "prairie")) %>%
  select(speciesID) %>% unlist() %>% unname()

veg_19_p <- veg_19 %>%
  filter(species %in% pra_codes)

list_19_p <- vector("list", length(list_19_names))
names(list_19_p) <- list_19_names 
for (i in seq_along(sites_19)) {            
  list_19_p[[i]] <- veg_19_p %>%
    filter(siteID == sites_19[[i]]) %>%
    select(species, starts_with(sites_19[[i]])) %>%
    tibble::column_to_rownames("species") %>%
    as.incfreq(.)
}
isites_19_p <- iNEXT(list_19_p, q=0, datatype = "incidence_freq")
plots_19_p <- ggiNEXT(isites_19_p, facet.var = "site")+
  labs(x = "Number of Quadrats",
       y = "Species Richness",
       title = "2019 sites rarefaction curves for prairie species only")+
  facet_wrap(~site, nrow = 5)+
  guides(lty = FALSE, color = FALSE, shape = FALSE, fill = FALSE)+
  theme_bw()+
  theme(axis.text = element_text(size = 10),
        strip.background = element_rect(fill = "white"))
ggsave("rarefaction_2019_p.png", plots_19_p, dpi = 500, width = 12, height = 9)
