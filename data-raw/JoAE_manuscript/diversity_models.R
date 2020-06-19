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

# Gamma prairie richness ------------------------------------------------------
# Richness of the prairie community at the site-level
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

# nix age
p1 <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + 
             season_seeded +
             (1|siteID), site_div_rich)
anova(p0, p1) # out!
anova(p1)

# nix season
p2 <- lmer(log(p_rich) ~ year + species_seeded + log(hectares_in_strips) + 
             (1|siteID), site_div_rich)
anova(p1, p2) # out!
anova(p2)

# p2 is the final model 
summary(p2)
confint.merMod(p2)
exp(0.017500)   
exp(0.017500*5)
# slope for species_seeded
(exp(0.017500)-1)*100         # percent increase
confint.merMod(p2) %>% exp()  # CI for percent increase
# CI for species_seeded
(exp(0.009461599)-1)*100         
(exp(0.02556925)-1)*100 

2^0.078368                   # slope for size (multiplicative for doubling size)
2^0.006143397
2^0.15059912

anova(p2)
performance::check_model(p2)
performance::r2(p2)
rand(p2)

x2s = c(-.7,0.4,1.61)
# equivalent to 0.5 hectares, 1.5 hectares, and 5 hectares
2.478392 + 0.080320*x2s[2]

slopes <- data.frame(int = c(2.422168, 2.51052, 2.607707),
                     sl  = c(0.016765, 0.016765, 0.016765),
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