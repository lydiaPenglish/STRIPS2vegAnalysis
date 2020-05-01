# looking at grass:forb ratio of the the seed mix 

library(dplyr)
library(ggplot2)
library(STRIPS2veg)

seed_divs <- readr::read_csv("data-raw/seed_mix_info/all_site_seed_div.csv")

# making dataframe
gf_rats <- seed_divs %>% 
  left_join(species_list, by = "speciesID") %>%
  mutate(seeds_per_lb   = seeds_per_oz*16,
         seeds_per_acre = seeds_per_lb*PLS_lbs_acre,
         seeds_per_sqft = seeds_per_acre/43560) %>%
  group_by(siteID, group_simple) %>%
  summarize(tot_seeds_sqft = sum(seeds_per_sqft)) %>%
  tidyr::pivot_wider(id_cols = siteID, names_from = group_simple, 
                     values_from = tot_seeds_sqft) %>%
  mutate(gf_ratio = `prairie grass`/`prairie forb`,
         tot_seeds = `prairie grass` + `prairie forb`)

# getting relative proportion of different functional groups
source("data-raw/00b_format_veg_cov_data.R")

# joining
gf_rat_weeds <- 
  left_join(gf_rats, weedy_pi, by = "siteID")

# Does grass forb ratio impact weed cover?
ggplot(gf_rat_weeds, aes(gf_ratio, weed_pi))+
  geom_point(size = 2)+
  facet_wrap(~year)+
  labs(x = "Grass: Forb Ratio",
       y = "Relative Proportion of Weeds")+
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

m1 <- lmerTest::lmer(car::logit(weed_pi) ~ gf_ratio + year + (1|siteID), gf_rat_weeds)
summary(m1)
ggResidpanel::resid_panel(m1)

# Does total number of seeds (seed density) impact weed cover?
ggplot(gf_rat_weeds, aes(tot_seeds, weed_pi))+
  geom_point()+
  facet_wrap(~year)
m2 <- lmerTest::lmer(car::logit(weed_pi) ~ tot_seeds + year + (1|siteID), gf_rat_weeds)
summary(m2)
ggResidpanel::resid_panel(m2)

# Does more grass seed = more grass cover?
gf_rat_prairie <- 
  left_join(gf_rats, prairie_pi, by = "siteID")

ggplot(gf_rat_prairie, aes(`prairie grass`, pg_pi))+
  geom_point()+
  facet_wrap(~year)
m3 <- lmerTest::lmer(car::logit(pg_pi) ~ `prairie grass` + year + (1|siteID), gf_rat_prairie)
summary(m3) # slightly significant

# What about forbs?
ggplot(gf_rat_prairie, aes(`prairie forb`, pf_pi))+
  geom_point()+
  facet_wrap(~year)

m4 <- lmerTest::lmer(car::logit(pf_pi) ~ `prairie forb` + year + (1|siteID), gf_rat_prairie)
summary(m4)
