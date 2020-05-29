# Script look at detection rate as well as relating pi of the seed mix to pi of the 
# vegetation stand

library(tidyverse)
library(STRIPS2veg)
library(extrafont)
data("species_list")
data("all_site_info")
data("veg_site")
data("extra_spp")
data("strips")

pa_rat <- strips %>%
  mutate(perim_area = perimeter/area) %>%
  group_by(siteID) %>%
  summarize(n_strips = n(),
            avg_p_a  = mean(perim_area))

all_site_info <- left_join(all_site_info, pa_rat)

nat_codes <- species_list %>% # 5 letter codes for prairie species
  filter(str_detect(group, "^prairie")) %>%
  dplyr::select(speciesID) %>%
  unlist()

# seed mix of each site 
sm <- read_csv("data-raw/seed_mix_info/all_site_seed_list.csv") # seed mix for each site

# adding in extras
pr_found <- 
  veg_site %>%
  filter(speciesID %in% nat_codes) %>%
  select(year, siteID, speciesID) %>%
  full_join(extra_spp)%>%                # adding in extra species
  filter(siteID != "GOS", siteID != "SME", siteID != "STT", siteID != "WAT") %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  arrange(siteID)

extras <- anti_join(pr_found, sm)     # species *not* seeded but still found

# detection rate
sm_found <- pr_found %>%
  anti_join(., extras) %>%            # get rid of species not seeded
  group_by(siteID) %>%
  summarize(n_found = n()) %>%
  left_join(., sm %>% group_by(siteID) %>% summarize(n_seeded = n())) %>%
  mutate(prop_found = n_found/n_seeded) %>%
  left_join(., all_site_info, by = "siteID")

# graph of the detection rate
p1 <- sm_found %>%
  mutate(siteID = fct_reorder(siteID, desc(prop_found))) %>%
  ggplot(aes(siteID, prop_found))+
  geom_point(aes(size = n_seeded))+
  labs(x = NULL, y = "Proportion of seed mix detected",
       size = "Number of \nspecies seeded")+
  coord_flip()+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        axis.text.y = element_text(face = "bold", size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.2)))+
  scale_y_continuous(limits = c(0.4, 1))
p1

sm_found %>%
  ggplot(aes(species_seeded, prop_found))+
  geom_point(size = 2, color = "#7A4FB6")+
  geom_hline(yintercept = 0.5, lty = 2)+
  scale_y_continuous(limits = c(0.25, 1), breaks = c(.25, 0.50, 0.75, 1))+
  labs(x = "Seedmix Richness",
       y = "Proportion of Seedmix \nDetected")+
  theme_bw() +
  theme(axis.title = element_text(size = 14, family = "Fira Sans"),
        axis.text = element_text(size = 12, family = "Fira Sans"))
  


# ---- Modeling detection rate as a function of covariate - NS bc of anything ----

l_full <- lm(car::logit(prop_found) ~ n_seeded + age_yrs + 
               log(acres_in_strips) + log(avg_p_a) + 
               season_seeded,
             data = sm_found)
anova(l_full)

# nix p_a ratio

l1 <- lm(car::logit(prop_found) ~ n_seeded + age_yrs + 
           log(acres_in_strips) + 
           season_seeded,
         data = sm_found)
anova(l1, l_full)      # out! 
anova(l1)

# get rid of age

l2 <- lm(car::logit(prop_found) ~ n_seeded + log(acres_in_strips) + 
           season_seeded,
         data = sm_found)
anova(l1, l2)          # out! 
anova(l2)

# get rid of season
l3 <- lm(car::logit(prop_found) ~ n_seeded  + log(acres_in_strips),
         data = sm_found)
anova(l2, l3)          # out!
anova(l3)

# get rid of size
l4 <- lm(car::logit(prop_found) ~ n_seeded,
         data = sm_found)
anova(l4, l3)

# also try just size 
l4 <- lm(car::logit(prop_found) ~ log(acres_in_strips),
         data = sm_found)
anova(l4)

# ---- Modeling detection rate as a function of weedy cover & richness - meh ----

weeds <- species_list %>% # 5 letter codes for weedy species
  filter(str_detect(group, "^weedy")) %>%
  select(speciesID) %>%
  unlist()

# weedy cover
weed_cov <-
  veg_site %>%
  filter(speciesID %in% weeds) %>%
  group_by(year, siteID, total_cover) %>%
  summarize(w_cov = sum(cov)) %>%
  mutate(w_pi = w_cov/total_cover) %>%
  group_by(siteID) %>%
  summarize(mean_cov  = mean(w_cov),
            mean_w_pi = mean(w_pi)) %>%
  left_join(sm_found)

#model
w1 <- lm(car::logit(prop_found) ~ mean_w_pi, data = weed_cov)
summary(w1)
ggResidpanel::resid_panel(w1)

wp1 <- weed_cov %>%
  ggplot(aes(mean_w_pi, car::logit(prop_found)))+
  geom_smooth(method = "lm", lty = 2, size = 2, color = "black")+
  geom_point(color = "grey20", size = 2)+
  labs(x = "Relative cover of \nweedy species",
       y = "logit(Detection rate)")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))
wp1

#  Richness
weed_rich <- veg_site %>%
  filter(speciesID %in% weeds) %>%
  ungroup() %>%
  select(siteID, speciesID) %>%
  distinct() %>%
  group_by(siteID) %>%
  summarize(w_rich = n()) %>%
  left_join(sm_found, by = "siteID")

w2 <- lm(car::logit(prop_found) ~ w_rich, weed_rich)
summary(w2)
anova(w2)
ggResidpanel::resid_panel(w2)

wp2 <- weed_rich %>%
  ggplot(aes(w_rich, car::logit(prop_found)))+
  geom_smooth(method = "lm", lty = 2, size = 2, color = "black")+
  geom_point(color = "grey20", size = 2)+
  labs(x = "Weed species richness",
       y = NULL)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title  = element_text(size = 14),
        axis.text.x = element_text(size = 12))
wp2

library(patchwork)
wp1 + wp2 + plot_annotation(tag_levels = 'A', tag_suffix = ")")

# ---- relative proportion species vs their seed mix ----

# I think this is ok....
seeded_pi <- read_csv("data-raw/seed_mix_info/all_site_seed_div.csv") %>%
  group_by(siteID) %>%
  mutate(tot_pls = sum(PLS_lbs_acre),
         seed_pi = PLS_lbs_acre/tot_pls)

sites_pi <- 
  left_join(seeded_pi, (filter(veg_site, year == "2019")))


sites_pi %>%
  filter(siteID == "SMI") %>%
  ggplot(aes(seed_pi, pi))+
  geom_text(aes(label = speciesID))

# ---- NOT USING: looking at BC between species seeded and species found for each site ----

# How similar are sites to their seed mixes?

#1. Presence/absence BC - not that interesting bc highly correlated with detection rate
sm_wide <- read_csv("data_raw/all_site_seed_lists.csv")%>%
  mutate(siteID = paste(siteID, "sm", sep = "_"))
sm_ids <- sm_wide %>% select(siteID) %>% unlist() %>% unname()

sp_wide <- sp_found %>%
  anti_join(., extras) %>%
  mutate(presence = 1) %>%
  pivot_wider(names_from = speciesID, values_from = presence)
sp_ids <- sp_wide %>% select(siteID) %>% unlist() %>% unname()

all_sites <- bind_rows(sm_wide, sp_wide) %>%
  arrange(siteID) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames("siteID")

all_bc_dist <- vegan::vegdist(all_sites, binary = TRUE)
all_bc <- reshape2::melt(as.matrix(all_bc_dist)) %>%
  mutate_at(vars(Var1, Var2), ~str_remove_all(., "\\_sm")) %>%
  filter(value != 0) %>%
  subset(., Var1 == Var2) %>%
  distinct(.) %>%
  rename("dissimilarity" = "value",
         "siteID"        = "Var1") %>%
  mutate(similarity = 1 - dissimilarity) %>%
  select(-Var2)

bc_sm <- left_join(all_bc, sm_found, by = "siteID")

bc_sm %>%
  ggplot(aes(prop_found, similarity))+
  geom_point()

