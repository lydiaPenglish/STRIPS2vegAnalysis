library(lmerTest)
library(tidyverse)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
data("all_site_info")
data("site_div_rich")

# 1.  ------ checking VIF of variables -----------------

dummy_var <- sample(1:100, 26)

# Model with only continuouse variables
c1 <- lm(dummy_var ~ species_seeded + age_yrs + log(acres_in_strips) + avg_p_a +
           season_seeded,
         na.action = "na.omit", data = filter(site_div_rich, year == "2019"))
summary(c1)
car::Anova(c1, type = "II")
car::vif(c1)

# 2A. ------ Gamma diversity models --------------------
data("site_div_rich")
hist(site_div_rich$gamma_div)

g_all <- lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(g_all)         # more variation in site than in residual
anova(g_all)

# nix perim_area rat
g1 <-  lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
              season_seeded +
              (1|siteID), data = site_div_rich)
anova(g_all, g1) # out!
anova(g1)

# nix season
g2 <-  lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + age_yrs +
              (1|siteID), data = site_div_rich)
anova(g1, g2) # out! (but close to mattering)
anova(g2)

# nix age
g3 <- lmer(gamma_div ~ year + species_seeded + log(acres_in_strips) + 
                  (1|siteID), data = site_div_rich)
anova(g2, g3)  # out!

# nix size
g4 <- lmer(gamma_div ~ year + species_seeded +
             (1|siteID), data = site_div_rich)
anova(g3, g4)  # keep! 

# final model = g3
resid_panel(g3)
summary(g3)
anova(g3)
performance::r2(g3)
performance::check_model(g3)
performance::model_performance(g3)
rand(g3)   # random effect matters

# aside... is there the same relatsionship with number of strips?
data("strips")
nos <- c("BUE_02", "GOS_04", "GOS_05", "GOS_06", "GOS_07", "ISB_02",
         "STT_01", "STT_02", "STT_03")
site_div_rich <- left_join(site_div_rich, 
                           strips %>%
                              filter(!(stripID %in% nos)) %>%
                              group_by(siteID) %>%
                              summarize(numStrips = n()),
                           by = "siteID")

site_div_rich %>%
  ggplot(aes(log(acres_in_strips), numStrips))+
  geom_text(aes(label = siteID))

t1 <- lm(log(acres_in_strips) ~ numStrips, site_div_rich)
summary(t1)

g3b <- lmer(gamma_div ~ year + species_seeded + numStrips + 
             (1|siteID), data = site_div_rich)
summary(g3b)


# 2B. ------ Beta diversity models ------------------------
hist(site_div_rich$beta_div)

b_all <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(b_all)
anova(b_all)

# nix p_a ratio

b0 <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + 
             age_yrs + season_seeded + 
             (1|siteID), data = site_div_rich)
anova(b0, b_all)   # out!
anova(b0)

# nix age
b1 <- lmer(beta_div ~ year + species_seeded + log(acres_in_strips) + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b_all, b1) # out!

# nix size
b2 <- lmer(beta_div ~ year + species_seeded + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b2, b1) # out!

# nix season
b3 <- lmer(beta_div ~ year + species_seeded + 
              (1|siteID), data = site_div_rich)
anova(b3, b2) # out! 

performance::compare_performance(b_all, b1, b2, b3)

# final model is b3
resid_panel(b3)
summary(b3)
anova(b3)
rand(b3) # random effect matters
performance::check_model(b3)
performance::r2(b3)

# 2C. ------ Alpha diveristy models ----------------------
data("quad_div_rich")
hist(quad_div_rich$alpha_div)

a_all <- lmer(alpha_div ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
summary(a_all)
anova(a_all)

# nix season seeded
a1 <-  lmer(alpha_div ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) + 
              age_yrs +
              (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
anova(a_all, a1) # out
anova(a1)

# nix p_a ratio

a2 <- lmer(alpha_div ~ year + species_seeded + log(acres_in_strips) +  
             age_yrs +
             (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
anova(a2, a1)
anova(a2)

# keep everything else, model is a2
resid_panel(a2)
summary(a2)
rand(a2)             # both random effects matter
performance::r2(a2)  # model explains less than gamma/beta diversity models
performance::check_model(a2)


# 3A. ------ Richness of the prairie community ---------------

hist(site_div_rich$p_rich)

p_all <- lmer(p_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
anova(p_all)
resid_panel(p_all)  # doesn't look great

site_div_rich <-
  site_div_rich %>%
  mutate(log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

p_log <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
summary(p_log)
anova(p_log)        # similar p-values to untransformed
resid_panel(p_log)  # better
resid_compare(list(p_all, p_log))
performance::compare_performance(p_all, p_log)

p_poi <- glmer(p_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                 age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)           # singular fit...

summary(p_poi)
resid_panel(p_poi)   # looks ok?

# dispersion parameter - manually
deviance(p_poi)/df.residual(p_poi)
r <- resid(p_poi, type = "pearson")
sum(r^2)/df.residual(p_poi)
# checking significance
1 - pchisq(deviance(p_poi), df.residual(p_poi))
# dispersion parameter - automatically                   # not overdispersed
performance::check_overdispersion(p_poi)

p_nb <- glmer.nb(p_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                   age_yrs + 
                   season_seeded +
                   (1|siteID), site_div_rich)     # also singular fit...
ggResidpanel::resid_panel(p_nb)
performance::check_model(p_nb)

# going to go ahead with log-transformed count data

anova(p_log)

# nix p a ratio

p0 <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + age_yrs + 
             season_seeded +
             (1|siteID), site_div_rich)
anova(p0, p_log)    # out!
anova(p0)

# nix age
p1 <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + 
             season_seeded +
             (1|siteID), site_div_rich)
anova(p0, p1) # out!
anova(p1)

# nix season
p2 <- lmer(log_p_rich ~ year + species_seeded + log(acres_in_strips) + 
             (1|siteID), site_div_rich)
anova(p1, p2) # out!
anova(p2)

# p2 is the final model 
summary(p2)
anova(p2)
performance::check_model(p2)
performance::r2(p2)

# 3B. ------ Richness of the weedy community -------------
hist(site_div_rich$w_rich)

w_all <- lmer(w_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
summary(w_all)
ggResidpanel::resid_panel(w_all)

w_poi <- glmer(w_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                 age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)        # model doesn't converge....grr

w_log <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
ggResidpanel::resid_panel(w_log)
anova(w_log)

# nix age
w1 <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) +
             log(avg_p_a) +
             season_seeded +
             (1|siteID), site_div_rich)
anova(w_log, w1)    # out!
anova(w1)

# nix season
w2 <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) +
             log(avg_p_a) +
             (1|siteID), site_div_rich)
anova(w2, w1)       # out!
anova(w2)

# nix p_a ratio
w3 <- lmer(log_w_rich ~ year + species_seeded + log(acres_in_strips) +
             (1|siteID), site_div_rich)
anova(w3, w2)    # out!
anova(w3)

# nix size
w4 <- lmer(log_w_rich ~ year + species_seeded + 
             (1|siteID), site_div_rich)
anova(w3, w4)       # out!

# w4 is the final model
anova(w4)
summary(w4)
performance::compare_performance(w_log, w1, w2, w3, w4)  # yup w4 is the best
performance::r2(w4)                                  # woof fixed effects explain little variation
performance::check_model(w4)

# 4A. ------ Alpha prairie richness ----------
quad_div_rich <- 
  quad_div_rich %>%
  mutate(p_rich     = replace(p_rich, p_rich == 0, 0.001),
         w_rich     = replace(w_rich, w_rich == 0, 0.001),
         log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

ap_all <- lmer(p_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + log(avg_p_a) +
                 season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
summary(ap_all)
anova(ap_all)
resid_panel(ap_all)

# nix p_a ratio

ap0 <- lmer(p_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + 
              season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(ap0, ap_all)     # out! 
anova(ap0)

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
anova(ap2)

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

# 4B. ------ Weedy spp richness -----------

wp_all <- lmer(w_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + 
                 log(avg_p_a) +
                 season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
resid_panel(wp_all)
anova(wp_all) # welp nothing signicant but will still go through stepwise

# nix p_a ratio
wp0 <- lmer(w_rich ~ year + species_seeded + age_yrs + log(acres_in_strips) + 
              season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp0, wp_all)    # out! 
anova(wp0)

# nix age 
wp1 <- lmer(w_rich ~ year + species_seeded + log(acres_in_strips) +
              season_seeded + 
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp1, wp0)  # out
anova(wp1)

# nix season
wp2 <- lmer(w_rich ~ year + species_seeded + log(acres_in_strips) +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp2, wp1) # out!
anova(wp2)

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

# 5.  ---- Mantel test looking at association between seed mix and all spp found ---- 

# Null H: No association between distance matrices
# Distance matrices = 
#     i) All seed mixes
#     ii) All species found

all_sm <- read_csv("data-raw/seed_mix_info/all_site_seed_list.csv")%>%
  mutate(presence = 1) %>%
  pivot_wider(names_from = "speciesID", values_from = "presence") %>%
  replace(is.na(.), 0) %>%
  column_to_rownames(var = "siteID")

data("veg_site")
all_veg <- 
  veg_site %>%
  ungroup() %>%
  select(siteID, speciesID) %>%
  filter(siteID != "WAT", siteID != "GOS", siteID != "STT", siteID != "SME") %>%
  distinct() %>%
  mutate(presence = 1) %>%
  arrange(speciesID)%>%
  pivot_wider(names_from = speciesID, values_from = presence) %>%
  replace(is.na(.), 0) %>%
  arrange(siteID) %>%
  column_to_rownames("siteID")

sm_dist  <- vegan::vegdist(all_sm, method = "bray")
veg_dist <- vegan::vegdist(all_veg, method = "bray")

vegan::mantel(sm_dist, veg_dist, permutations = 9999)
# statistic = 0.2596, p = 0.028



