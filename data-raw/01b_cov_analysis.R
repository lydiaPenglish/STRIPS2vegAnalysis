library(lmerTest)
library(dplyr)
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



performance::compare_performance(w1, w2)

# 2B. Weedy perennials - both ok diagnostically, but logit fits better

wp1 <- lmer(avg_cov ~  year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), pw_cov)
performance::check_model(wp1)

wp2 <- lmer(wp_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), weedy_pi)
performance::check_model(wp2)

performance::compare_performance(wp1, wp2)

summary(wp1)

# 2C. Weedy annuals - logit transformation of relative cover

wa1 <- lmer(avg_cov ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), aw_cov)
summary(wa1)
performance::check_model(wa1)

wa2 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + log(area_in_strips) + season_seeded + 
              (1|siteID), weedy_pi)
summary(wa2)
performance::check_model(wa2)

performance::compare_performance(wa1, wa2)

