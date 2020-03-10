library(lmerTest)
library(tidyverse)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)

source("data-raw/00b_format_veg_cov_data.R")

# Modeling avg cover of prairie grasses, prairie forbs, annual weeds, and perennial weeds

# 1A. All prairie spp - NS

p1 <- lmer(prairie_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
             (1|siteID), prairie_pi)
summary(p1)
anova(p1)

# nix age
p2 <- lmer(prairie_pi_logit ~ year + species_seeded + log(area_in_strips) + season_seeded + 
             (1|siteID), prairie_pi)
anova(p1, p2)     # out!

# nix season
p3 <- lmer(prairie_pi_logit ~ year + species_seeded + log(area_in_strips) + 
             (1|siteID), prairie_pi)
anova(p2, p3)     # out!

# nix size
p4 <- lmer(prairie_pi_logit ~ year + species_seeded +
                   (1|siteID), prairie_pi)
anova(p3, p4)

#final model = p4
summary(p4)
anova(p4)
rand(p4)     # effect matters
performance::r2(p4)
performance::check_model(p4)
performance::compare_performance(p1, p2, p3, p4)

# 1B. Prairie grass - NS

g1 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
             (1|siteID), prairie_pi)
summary(g1)
anova(g1)

# nix season
g2 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) +
             (1|siteID), prairie_pi)
anova(g2, g1)     # out!

# nix size
g3 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + 
             (1|siteID), prairie_pi)
anova(g3, g2)     # out!

# nix age
g4 <- lmer(pg_pi_logit ~ year + species_seeded + 
             (1|siteID), prairie_pi)
anova(g4, g3)

# final model is g4
anova(g4)
summary(g4)
performance::r2(g4)
rand(g4)
performance::check_model(g4)

# 1C. Prairie forb 

f1 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
             (1|siteID), prairie_pi)
anova(f1)

# nix size
f2 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + season_seeded + 
             (1|siteID), prairie_pi)
anova(f2, f1)      # out!

# nix season
f3 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + 
             (1|siteID), prairie_pi)

anova(f3, f2)      # out! 

# nix age
f4 <- lmer(pf_pi_logit ~ year + species_seeded + 
             (1|siteID), prairie_pi)
anova(f4, f3)     # out!

# f4 is the model to use
summary(f4)
rand(f4)
anova(f4)
performance::r2(f4)
performance::check_model(f4)

# 2A. All weedy spp
w1 <- lmer(weed_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
          (1|siteID), weedy_pi)
anova(w1)

# nix age
w2 <- lmer(weed_pi_logit ~ year + species_seeded + log(area_in_strips) + season_seeded + 
             (1|siteID), weedy_pi)
anova(w2, w1)       # out!

# nix season
w3 <- lmer(weed_pi_logit ~ year + species_seeded + log(area_in_strips) +  
            (1|siteID), weedy_pi)
anova(w2, w3)       # out!

# nix size
w4 <-  lmer(weed_pi_logit ~ year + species_seeded +
              (1|siteID), weedy_pi)
anova(w3, w4)       # out!

# w4 is the final model
summary(w4)
anova(w4)
performance::check_model(w4)
performance::r2(w4)
rand(w4)

# 2B. Weedy perennials - both ok diagnostically, but logit fits better

wp1 <- lmer(wp_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), weedy_pi)
performance::check_model(wp1)
anova(wp1)

# nix age
wp2 <- lmer(wp_pi_logit ~ year + species_seeded + log(area_in_strips) + season_seeded + 
              (1|siteID), weedy_pi)
anova(wp1, wp2)           # out!

# nix season
wp3 <- lmer(wp_pi_logit ~ year + species_seeded + log(area_in_strips) + 
              (1|siteID), weedy_pi)
anova(wp2, wp3)           # out!

# nix size
wp4 <- lmer(wp_pi_logit ~ year + species_seeded + 
              (1|siteID), weedy_pi)
anova(wp3, wp4)           # out!

# wp4 is the final model
summary(wp4)
anova(wp4)
performance::check_model(wp4)
performance::r2(wp4)
rand(wp4)

# 2C. Weedy annuals - logit transformation of relative cover

wa1 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), weedy_pi)
anova(wa1)

# nix size

wa2 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + season_seeded + 
              (1|siteID), weedy_pi)
anova(wa2, wa1)            # out!

# nix season
wa3 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + 
              (1|siteID), weedy_pi)
anova(wa3, wa2)            # out!

# nix age
wa4 <- lmer(wa_pi_logit ~ year + species_seeded +
              (1|siteID), weedy_pi)
anova(wa4, wa3)            # keep!

# final model is wa3
summary(wa3)
anova(wa3)
performance::check_model(wa3)
performance::r2(wa3)
rand(wa3)                  # hmmm site doesn't matter...should I not include it? 

# alternative...
wa5 <- lm(wa_pi_logit ~ year + species_seeded + age_yrs, weedy_pi)
summary(wa5)
anova(wa5)

# ---- prairie vs weedy cover/richness ----
data("site_div_rich")

pra_vs_wd <- left_join(site_div_rich, prairie_pi)

ggplot(pra_vs_wd, aes(prairie_pi, log(w_rich)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

pw1 <- lmer(log(w_rich) ~ year + prairie_pi + (1|siteID), pra_vs_wd)
summary(pw1)
anova(pw1)
performance::check_model(pw1)
performance::r2(pw1)

wd_vs_pra <- left_join(site_div_rich, weedy_pi)

ggplot(wd_vs_pra, aes(weed_pi, log(p_rich)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

wp1 <- lmer(log(p_rich) ~ year + weed_pi + (1|siteID), wd_vs_pra)
summary(wp1)
anova(wp1)
performance::check_model(wp1)
performance::r2(wp1)

ggplot(site_div_rich, aes(p_rich, w_rich))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

wpr <- lmer(log(w_rich) ~ year + p_rich + (1|siteID), site_div_rich)
summary(wpr)
rand(wpr)
