library(lmerTest)
library(dplyr)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
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
performance::r2(g2)
performance::check_model(g2)
performance::model_performance(g2)
rand(g2)   # random effect matters

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

performance::compare_performance(b_all, b1, b2, b3)

# final model is b1
resid_panel(b1)
summary(b1)
anova(b1)
rand(b1) # random effect matters
performance::check_model(b1)
performance::r2(b1)

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
performance::r2(a1)  # model explains less than gamma/beta diversity models
performance::check_model(a1)


# 3A. Richness of the prairie community

hist(site_div_rich$p_rich)

p_all <- lmer(p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
anova(p_all)
resid_panel(p_all)  # doesn't look great

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

# dispersion parameter - manually
deviance(p_poi)/df.residual(p_poi)
r <- resid(p_poi, type = "pearson")
sum(r^2)/df.residual(p_poi)
# checking significance
1 - pchisq(deviance(p_poi), df.residual(p_poi))
# dispersion parameter - automatically                   # not overdispersed
performance::check_overdispersion(p_poi)

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
performance::check_model(p2)

# 3B. Richness of the weedy community
hist(site_div_rich$w_rich)

w_all <- lmer(w_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich)
summary(w_all)
ggResidpanel::resid_panel(w_all)

w_poi <- glmer(w_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)        # model doesn't converge....grr

w_log <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
ggResidpanel::resid_panel(w_log)
anova(w_log)

# nix age
w1 <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) +
             season_seeded +
             (1|siteID), site_div_rich)
anova(w_log, w1)    # out!
anova(w1)

# nix season
w2 <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) +
             (1|siteID), site_div_rich)
anova(w2, w1)       # out!
anova(w2)

# nix size
w3 <- lmer(log_w_rich ~ year + species_seeded + 
             (1|siteID), site_div_rich)
anova(w3, w2)       # out!

# w3 is the final model
anova(w3)
summary(w3)
# I feel like i need to back-transform these values...
performance::compare_performance(w_log, w1, w2, w3)  # yup w3 is the best
performance::r2(w3)                                  # woof fixed effects explain little variation
performance::check_model(w3)

# 4A. --- Alpha prairie richness ---
quad_div_rich <- 
  quad_div_rich %>%
  mutate(p_rich     = replace(p_rich, p_rich == 0, 0.001),
         w_rich     = replace(w_rich, w_rich == 0, 0.001),
         log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

ap_all <- lmer(p_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
summary(ap_all)
anova(ap_all)
resid_panel(ap_all)

# nix area
ap1 <- lmer(p_rich ~ year + species_seeded + age_yrs + season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(ap1, ap_all)     # out!

# nix season 
ap2 <- lmer(p_rich ~ year + species_seeded + age_yrs +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)        # model failed to converge?
anova(ap1, ap2) # idk how this still works, but out!

# nix age
ap3 <- lmer(p_rich ~ year + species_seeded +
              (1|quadratID:siteID) + (1|siteID), quad_div_rich)
anova(ap3, ap2) # keep age...

# model ap2 is the model, but it doesn't converge?
performance::check_convergence(ap2)           # this says it did converge so...
performance::r2(ap2)
summary(ap2)
anova(ap2)
rand(ap2)   # both random effects matter
performance::check_model(ap2)                 # idk some of these plots look weird but I'm going with it


# just checking poisson and log-transformed - sticking with assumptions of normality and not-transforming
ap2_log <- lmer(log_p_rich ~ year + species_seeded + age_yrs +
                  (1|quadratID:siteID) + (1|siteID), quad_div_rich)
performance::check_model(ap2_log)                 # lower AIC but looks terrible, keeping with normal
ap2_poi <- glmer(p_rich ~ year + species_seeded + age_yrs +
                   (1|quadratID:siteID) + (1|siteID), quad_div_rich,
                 family = poisson)
performance::check_overdispersion(ap2_poi)        # poisson is ok
performance::compare_performance(ap2, ap2_log, ap2_poi)

# 4B. --- Weedy spp richness ---

wp_all <- lmer(w_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
resid_panel(wp_all)
anova(wp_all) # welp nothing signicant but will still go through stepwise

# nix season
wp1 <- lmer(w_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp_all, wp1) # out!

# nix age 
wp2 <- lmer(w_rich ~ year + species_seeded + log(acres_in_strips) +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp1, wp2)  # out

# nix size 
wp3 <-  lmer(w_rich ~ year + species_seeded + 
               (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp3, wp2)  # out

# wp3 is the final model
anova(wp3)
summary(wp3)
rand(wp3)        # both random effects matter
performance::check_model(wp3)
performance::r2(wp3)     # ha, fixed effects explain very little...
