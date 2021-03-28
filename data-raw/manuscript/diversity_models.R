# Models looking at gamma, beta, and alpha diversity and richness

# Load libraries and datasets --------------------------------------------------
library(lmerTest)
library(dplyr)
library(ggplot2)
# remotes::install_github("lydiaPenglish/STRIPSveg")  
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
library(emmeans)

data("all_site_info")          # from STRIPS2veg
data("site_div_rich")          # from this repo, created in "data-raw/00_format_veg_div_data.R"
data("quad_div_rich")          # from this repo, created in "data-raw/00_format_veg_div_data.R"

# checking VIF/association of x variables ---------------------------------------

dummy_var <- sample(1:100, 26)

# Model with only continuouse variables
c1 <- lm(dummy_var ~ species_seeded + age_yrs + log(hectares_in_strips) + avg_p_a +
           season_seeded,
         na.action = "na.omit", data = filter(site_div_rich, year == "2019"))
summary(c1)
car::Anova(c1, type = "II")
car::vif(c1)

# relationship between age and species seeded: 

s18 <- lm(species_seeded ~ age_yrs, site_div_rich %>% filter(year == "2018"))
summary(s18)

s19 <- lm(species_seeded ~ age_yrs, site_div_rich %>% filter(year == "2019"))
summary(s19)

site_div_rich %>%
  ggplot(aes(age_yrs, species_seeded))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

# weak-ish but still associated! 

# other graphs:

# age vs edginess - meh
site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(age_yrs, log(avg_p_a)))+
  geom_point()+
  geom_smooth(method = "lm")

# size vs species seeded - meh 
site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, log(hectares_in_strips)))+
  geom_point()+
  geom_smooth(method = "lm")

# Gamma diversity models ----------------------------------------------------------
hist(site_div_rich$gamma_div)

g_all <- lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(g_all)         # more variation in site than in residual
anova(g_all)
resid_panel(g_all)

# nix perim_area rat
g1 <-  lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + age_yrs +
              season_seeded +
              (1|siteID), data = site_div_rich)
anova(g_all, g1) # out!
anova(g1)

# nix season
g2 <-  lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + age_yrs +
              (1|siteID), data = site_div_rich)
anova(g1, g2) # out! (but close to mattering)
anova(g2)

# nix age
g3 <- lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + 
             (1|siteID), data = site_div_rich)
anova(g2, g3)  # out!

# nix size
g4 <- lmer(gamma_div ~ year + species_seeded +
             (1|siteID), data = site_div_rich)
anova(g3, g4)  # keep! 

performance::compare_performance(g_all, g1, g2, g3, g4)

# final model = g3
resid_panel(g3)
summary(g3)
performance::r2(g3)
rand(g3)   # random effect matters
confint.merMod(g3)   # 95% CIs

# slopes = 0.371 (species seeded) and 2.58 (log(hectares_in_strips))
2.58710*log(2) # change in diveristy when area in doubled
0.9436*log(2)
4.23006*log(2)

# plotting predictions on 2019 data
xs = c(-.7,0.4,1.61)
# equivalent to 0.5 hectares, 1.5 hectares, and 5 hectares (log each of those numbers)

intercept <- (5.33272 + 1.38303)   # for 2019 
size_sl <- 2.587102
intercept + size_sl*xs[1]
intercept + size_sl*xs[2]
intercept + size_sl*xs[3]

slopes <- data.frame(int = c(4.899029, 7.744841, 10.87523),
                     sl  = c(0.3707452, 0.3707452, 0.3707452),
                     id  = c("0.5 ha", "1.5 ha", "5 ha"))

site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, gamma_div))+
  geom_point(alpha = 0.5, size = 2)+
  geom_abline(data = slopes, aes(intercept = int, slope = sl, color = id),
              size = 1.5)+
  scale_color_manual(values = c("#5F1343", "#A41393", "#C669D5"),
                     guide = guide_legend(reverse = TRUE))+
  labs(x = "Seed mix richness",
       y = "Gamma diversity (e^H')") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Gamma prairie richness ------------------------------------------------------
# aka Richness of the prairie community at the site-level
hist(site_div_rich$p_rich)

p_all <- lmer(p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
resid_panel(p_all)  # doesn't look great ? Try a log transform...

p_log <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
resid_compare(list(p_all, p_log))                 # log transformation looks slightly better
performance::compare_performance(p_all, p_log)    # how is the log AIC *so much* lower?

anova(p_all)
anova(p_log)        # similar p-values to untransformed

# try a poisson model too
p_poi <- glmer(p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                 age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)           # singular fit...
summary(p_poi)       # hmmm no site variance, probably why singular
resid_panel(p_poi)   # looks ok?
performance::check_overdispersion(p_poi)    # not overdispersed

# going to go ahead with log-transformed count data to keep things normal

anova(p_log)
# nix p a ratio

p0 <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + age_yrs + 
             season_seeded +
             (1|siteID), site_div_rich)
anova(p0, p_log)    # out!
anova(p0)

# nix season
p1 <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + 
             age_yrs +
             (1|siteID), site_div_rich)
anova(p0, p1) # out!
anova(p1)

# nix age
p2 <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + 
             (1|siteID), site_div_rich)
anova(p1, p2) # out!
anova(p2)

# p2 is the final model 
performance::compare_performance(p0, p1, p2, p_log)
summary(p2)
confint.merMod(p2)
exp(0.017500)   
# slope for species_seeded
(exp(0.017500)-1)*100         # percent increase
# CI for species_seeded
(exp(0.009461599)-1)*100         
(exp(0.02556925)-1)*100 

2^0.078368                   # slope for size (multiplicative for doubling size)
2^0.006143397                # CI for size (multiplicative for doubling size)
2^0.15059912

resid_panel(p2)
performance::r2(p2)
rand(p2)

# making plot
xs = c(-.7,0.4,1.61)
# equivalent to 0.5 hectares, 1.5 hectares, and 5 hectares
int <- 2.480545 - 0.044179

int + 0.078368 *xs[1]
int + 0.078368 *xs[2]
int + 0.078368 *xs[3]

slopes <- data.frame(int = c(2.381508, 2.467713, 2.562538),
                     sl  = c(0.017500,  0.017500, 0.017500 ),
                     id  = c("0.5 ha", "1.5 ha", "5 ha"))

site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, p_rich))+
  geom_point(alpha = 0.5, size = 2)+
  geom_abline(data = slopes, aes(intercept = int, slope = sl, color = id),
              size = 1.5)+
  scale_color_manual(values = c("#5F1343", "#A41393", "#C669D5"),
                     guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(trans = "log", breaks = c(10, 20, 30), limits = c(10, 40))+
  labs(color = NULL,
       x = "Seed mix richness", 
       y = "Prairie species richness")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Gamma weedy richness -----------------------------------------------------
# aka site level richness of the weeds

hist(site_div_rich$w_rich)
w_all <- lmer(w_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
summary(w_all)
resid_panel(w_all)

w_poi <- glmer(w_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                 age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)        # model doesn't converge....grr
performance::check_convergence(w_poi)   # idk this says it's ok? 

w_log <- lmer(log(w_rich) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
resid_panel(w_log)
resid_compare(list(w_all, w_log))              # Hmm log transform might be a little worse? 

# still going forward with log
anova(w_log)
# nix age
w1 <- lmer(log(w_rich) ~ year + species_seeded + log(hectares_in_strips) +
             log(avg_p_a) +
             season_seeded +
             (1|siteID), site_div_rich)
anova(w_log, w1)    # out!
anova(w1)

# nix p_a ratio
w2 <- lmer(log(w_rich) ~ year + species_seeded + log(hectares_in_strips) +
             season_seeded +
             (1|siteID), site_div_rich)
anova(w2, w1)       # out!
anova(w2)

# nix season
w3 <- lmer(log(w_rich) ~ year + species_seeded + log(hectares_in_strips) +
             (1|siteID), site_div_rich)
anova(w3, w2)    # out!
anova(w3)

# nix size
w4 <- lmer(log(w_rich) ~ year + species_seeded + 
             (1|siteID), site_div_rich)
anova(w3, w4)       # out!

# w4 is the final model
anova(w4)
summary(w4)                                    # NS trend that weed richness increases with seed mix richness
confint.merMod(w4)
performance::compare_performance(w_log, w1, w2, w3, w4)  # yup w4 is the best
performance::r2(w4)                                  # woof fixed effects explain little variation
resid_panel(w4)
rand(w4)

# plot
site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, w_rich))+
  geom_point(alpha = 0.5, size = 2)+
  geom_abline(intercept = 2.498264, slope = 0.012534,
              size = 1.5)+
  #scale_color_manual(values = c("#5F1343", "#A41393", "#C669D5"),
  #                  guide = guide_legend(reverse = TRUE))+
  scale_y_continuous(trans = "log", breaks = c(10, 20, 30), limits = c(10, 40))+
  labs(color = NULL,
       x = "Seed mix richness", 
       y = "Weedy species richness")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Variation across a site ------------------------------------------------------

# CV of diversity

alpha_cv <- quad_div_rich %>%
  group_by(year, siteID) %>%
  summarize(alpha_avg = mean(alpha_div),
            alpha_sd = sd(alpha_div),
            alpha_cv = alpha_sd/alpha_avg)

# Joining

site_div_rich3 <- left_join(site_div_rich, alpha_cv)

site_div_rich3 %>%
  ggplot() +
  geom_point(aes(evenness, alpha_cv))+
  geom_label(aes(evenness, alpha_cv, label = siteID))+
  geom_smooth(aes(evenness, alpha_cv), method = "lm")+
  facet_wrap(~year)

# Just looking at cv
hist(car::logit(site_div_rich3$alpha_cv))

cv_all <- lmer(car::logit(alpha_cv) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich3)
summary(cv_all)

# nix season
cv0 <- lmer(car::logit(alpha_cv) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
             age_yrs + 
             (1|siteID), data = site_div_rich3)
anova(cv_all, cv0) # out!
summary(cv0)

# nix age
cv1 <- lmer(car::logit(alpha_cv) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
              (1|siteID), data = site_div_rich3)
anova(cv1, cv0) #out!
summary(cv1)

# nix p_a

cv2 <- lmer(car::logit(alpha_cv) ~ year + species_seeded + log(hectares_in_strips) + 
              (1|siteID), data = site_div_rich3)
anova(cv2, cv1) #technically out
summary(cv2)

# try nixing size 
e3 <-  lmer(evenness ~ year + species_seeded + 
              (1|siteID), data = site_div_rich3)
anova(e2, e3) # keep!
summary(e3)

# final model is e2 
resid_panel(e1)
summary(e1)
confint.merMod(e1)
rand(e1) # random effect matters
performance::r2(e1)

# Just looking at evenness 

site_div_rich3 %>%
  ggplot(aes(log(hectares_in_strips), evenness))+
  geom_point()+
  facet_wrap(~year)

e_all <- lmer(car::logit(evenness) ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
             age_yrs + season_seeded + 
             (1|siteID), data = site_div_rich3)
summary(e_all)

# nix p_a
e0 <- lmer(car::logit(evenness) ~ year + species_seeded + log(hectares_in_strips) + 
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich3)
anova(e_all, e0) # out!
summary(e0)

# nix age
e1 <- lmer(car::logit(evenness) ~ year + species_seeded + log(hectares_in_strips) + 
              season_seeded + 
             (1|siteID), data = site_div_rich3)
anova(e1, e0) #out!
summary(e1)

# trying nixing season_seeded

e2 <- lmer(car::logit(evenness) ~ year + species_seeded + log(hectares_in_strips) + 
             (1|siteID), data = site_div_rich3)
anova(e2, e1) #technically out
summary(e2)

# try nixing size 
e3 <-  lmer(evenness ~ year + species_seeded + 
              (1|siteID), data = site_div_rich3)
anova(e2, e3) # keep!
summary(e3)

# final model is e2 
resid_panel(e1)
summary(e1)
confint.merMod(e1)
rand(e1) # random effect matters
performance::r2(e1)

hist(car::logit(site_div_rich3$evenness))

# Beta diversity models ---------------------------------------------
hist(site_div_rich$beta_div)

b_all <- lmer(beta_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(b_all)
resid_panel(b_all)
anova(b_all)

# nix p_a ratio

b0 <- lmer(beta_div ~ year + species_seeded + log(hectares_in_strips) + 
             age_yrs + season_seeded + 
             (1|siteID), data = site_div_rich)
anova(b0, b_all)   # out!
anova(b0)

# nix age
b1 <- lmer(beta_div ~ year + species_seeded + log(hectares_in_strips) + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b_all, b1) # out!
anova(b1)

# nix size
b2 <- lmer(beta_div ~ year + species_seeded + 
             season_seeded + (1|siteID), data = site_div_rich)
anova(b2, b1) # oooof, should nearly keep but out! 
anova(b2)

# nix season
b3 <- lmer(beta_div ~ year + species_seeded + 
             (1|siteID), data = site_div_rich)
anova(b3, b2) # out! 

performance::compare_performance(b_all, b1, b2, b3)

# final model is b3
resid_panel(b3)
summary(b3)
confint.merMod(b3)
rand(b3) # random effect matters
performance::r2(b3)

#plot
site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, beta_div))+
  geom_point(alpha = 0.5, size = 2)+
  geom_abline(intercept = 1.59061, slope = 0.04548, size = 1.5, color = "#C669D5") +
  theme_bw()+
  labs(y = "Beta Diversity",
       x = "Seed mix richness")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Alpha diversity models ------------------------------------------------------
hist(quad_div_rich$alpha_div)

a_all <- lmer(alpha_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
resid_panel(a_all)
summary(a_all)
anova(a_all)

# nix season seeded
a1 <-  lmer(alpha_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) + 
              age_yrs +
              (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
anova(a_all, a1) # out
anova(a1)

# nix p_a ratio

a2 <- lmer(alpha_div ~ year + species_seeded + log(hectares_in_strips) +  
             age_yrs +
             (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
anova(a2, a1)
anova(a2)

performance::compare_performance(a_all, a1, a2)

# keep everything else, model is a2
resid_panel(a2)
summary(a2)
confint.merMod(a2) 
performance::r2(a2)  # model explains less than gamma/beta diversity models
rand(a2)             # both random effects matter

0.710130 * log(2)    # slope estimate for area, getting doubling effect
confint.merMod(a2) * log(2)


# plotting slope estimates
xs = c(-.7,0.4,1.61)
intercept <- (9.450204 + 0.120361)
size_sl <- 0.710130
intercept + size_sl*xs[1]
intercept + size_sl*xs[2]
intercept + size_sl*xs[3]

slopes <- data.frame(int = c(9.073474, 9.854617, 10.71387),
                     sl  = c(-0.627538, -0.627538, -0.627538),
                     id  = c("0.5 ha", "1.5 ha", "5 ha"))

# plot 
quad_div_rich %>%
  filter(year == "2019" & !(is.na(species_seeded))) %>%
  group_by(siteID) %>%
  mutate(avg_alpha = mean(alpha_div)) %>%
  ggplot(aes(age_yrs, alpha_div))+
  geom_point(alpha = 0.15)+
  geom_point(aes(age_yrs, avg_alpha), size = 4)+
  geom_abline(data = slopes, aes(intercept = int, slope = sl, color = id), 
              size = 1.5) +
  scale_color_manual(values = c("#5F1343", "#A41393", "#C669D5"),
                     guide = guide_legend(reverse = TRUE))+
  theme_bw()+
  labs(y = "Alpha Diversity",
       x = "Age (years)",
       color = NULL)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

# Alpha prairie richness -------------------------------------------
# species richness of prairie species at the quadrat level

ap_all <- lmer(p_rich ~ year + species_seeded + age_yrs + log(hectares_in_strips) + log(avg_p_a) +
                 season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
summary(ap_all)
anova(ap_all)
resid_panel(ap_all)

# nix size

ap0 <- lmer(p_rich ~ year + species_seeded + age_yrs +  log(avg_p_a) +
              season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(ap0, ap_all)     # out! 
anova(ap0)

# nix ap ratio
ap1 <- lmer(p_rich ~ year + species_seeded + age_yrs +  season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(ap1, ap_all)     # out!
anova(ap1)

# nix age
ap2 <- lmer(p_rich ~ year + species_seeded + season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)        # model failed to converge?
anova(ap1, ap2) # idk how this still works, but out!
anova(ap2)

# nix season
ap3 <- lmer(p_rich ~ year + species_seeded +
              (1|quadratID:siteID) + (1|siteID), quad_div_rich)
anova(ap3, ap2) # keep season

#final model = ap2
anova(ap2)
summary(ap2)

ap2_m <- emmeans(ap2, ~ season_seeded)
pairs(ap2_m)
contrast(ap2_m)

performance::r2(ap2)
summary(ap2)
confint.merMod(ap2)
anova(ap2)
rand(ap2)   # both random effects matter
resid_panel(ap2)            

# fitting glmer too
ap2_poi <- glmer(p_rich ~ year + species_seeded + age_yrs +
                   (1|quadratID:siteID) + (1|siteID), quad_div_rich,
                 family = poisson)    
performance::check_convergence(ap2_poi)           # says its ok
performance::check_overdispersion(ap2_poi)        # poisson is ok
performance::compare_performance(ap2, ap2_poi)
summary(ap2_poi)

# Alpha weedy richness -----------------------------------------------
# aka species richness of the weed on the quadrat scale

wp_all <- lmer(w_rich ~ year + species_seeded + age_yrs + log(hectares_in_strips) + 
                 log(avg_p_a) +
                 season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
resid_panel(wp_all)
anova(wp_all) # welp nothing signicant but will still go through stepwise

# nix p_a ratio
wp0 <- lmer(w_rich ~ year + species_seeded + age_yrs + log(hectares_in_strips) + 
              season_seeded +
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp0, wp_all)    # out! 
anova(wp0)

# nix age 
wp1 <- lmer(w_rich ~ year + species_seeded + log(hectares_in_strips) +
              season_seeded + 
              (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
anova(wp1, wp0)  # out
anova(wp1)

# nix season
wp2 <- lmer(w_rich ~ year + species_seeded + log(hectares_in_strips) +
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
confint.merMod(wp3)
rand(wp3)        # both random effects matter
performance::compare_performance(wp_all, wp0, wp1, wp2, wp3)

resid_panel(wp3)
performance::r2(wp3)     # ha, fixed effects explain very little...
