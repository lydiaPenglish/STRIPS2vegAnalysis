library(lmerTest)
library(car)
library(dplyr)
library(STRIPS2veg)
library(ggResidpanel)
data("all_site_info")

# 1. checking VIF of variables 

dummy_var <- sample(1:100, 25)

# Model with only continuouse variables
c1 <- lm(dummy_var ~ species_seeded + age_yrs + log(acres_in_strips) + season_seeded,
         na.action = "na.omit", data = all_site_info)
summary(c1)
Anova(c1, type = "II")
car::vif(c1)

# 2A. Gamma diversity models 
data("site_div_rich")
hist(site_div_rich$gamma_div)

g_all <- lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
                season_seeded + (1|siteID), data = site_div_rich)
summary(g_all)         # more variation in site than in residual
anova(g_all)

# nix season
g1 <-  lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
               (1|siteID), data = site_div_rich)
anova(g_all, g1) # out!

# nix age
g2 <- lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + 
                  (1|siteID), data = site_div_rich)
anova(g2, g1)  # out!

# nix size
g3 <- lmer(gamma_div ~ year + species_seeded +
             (1|siteID), data = site_div_rich)
anova(g3, g2)  # keep! 

# final model = g2
resid_panel(g2)
summary(g2)
anova(g2)
rand(g_all)   # random effect matters

# 2B. Beta diversity models
hist(site_div_rich$beta_div)

b_all <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
                season_seeded + (1|siteID), data = site_div_rich)
summary(b_all)
anova(b_all)

# nix age
b1 <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b_all, b1) # out!

# nix size
b2 <- lmer(beta_div ~ year + species_seeded + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b2, b1) # keep!

# nix season
b3 <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + 
              (1|siteID), data = site_div_rich)
anova(b3, b1) # keep! 

# final model is b1
resid_panel(b1)
summary(b1)
anova(b1)
rand(b1) # random effect matters

# 2C. Alpha diveristy models
data("quad_div_rich")
hist(quad_div_rich$alpha_div)

a_all <- lmer(alpha_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
                season_seeded + 
                (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
summary(a_all)
anova(a_all)

# nix season seeded
a1 <-  lmer(alpha_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
              (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
anova(a_all, a1) # out
anova(a1)

# keep everything else, model is a1
resid_panel(a1)
summary(a1)
rand(a1)             # both random effects matter

# 3A. Richness of the prairie community

hist(site_div_rich$p_rich)

p_all <- lmer(p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
anova(p_all)
resid_panel(p_all) # ooof

site_div_rich <-
  site_div_rich %>%
  mutate(log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

p_log <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
summary(p_log)
anova(p_log)        # similar p-values to untransformed
resid_panel(p_log)  # better

p_poi <- glmer(p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)           # singular fit...
summary(p_poi)
resid_panel(p_poi)
1 - pchisq(deviance(p_poi), df.residual(p_poi))     # Hmmm not close to 1, underdispersed?

# dispersion parameter
deviance(p_poi)/df.residual(p_poi)
r <- resid(p_poi, type = "pearson")
sum(r^2)/df.residual(p_poi)

p_nb <- glmer.nb(p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                   season_seeded +
                   (1|siteID), site_div_rich)     # also singular fit...

# going to go ahead with log-transformed count data

# nix season
p1 <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
             (1|siteID), site_div_rich)
anova(p1, p_log) # out!

# nix age
p2 <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + 
             (1|siteID), site_div_rich)
anova(p2, p1) # out!

# p2 is the final model 
summary(p2)
anova(p2)

# 3B. Richness of the weedy community
hist(site_div_rich$w_rich)

