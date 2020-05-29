library(lmerTest)
library(tidyverse)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
library(extrafont)
data("all_site_info")
data("site_div_rich")

# 1.  ------ checking VIF of variables -----------------

dummy_var <- sample(1:100, 26)

# Model with only continuouse variables
c1 <- lm(dummy_var ~ species_seeded + age_yrs + log(hectares_in_strips) + avg_p_a +
           season_seeded,
         na.action = "na.omit", data = filter(site_div_rich, year == "2019"))
summary(c1)
car::Anova(c1, type = "II")
car::vif(c1)

# 2A. ------ Gamma diversity models --------------------
data("site_div_rich")
hist(site_div_rich$gamma_div)

g_all <- lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(g_all)         # more variation in site than in residual
anova(g_all)

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
confint.merMod(g3) 
# checking model...
performance::r2(g3)
performance::check_model(g3)
performance::model_performance(g3)
rand(g3)   # random effect matters

  # slopes = 0.371 (species seeded) and 2.58 (log(hectares_in_strips))
2.58710*log(2) # change in diveristy when area in doubled
0.9436*log(2)
4.23006*log(2)

2.58710*log(1.471869) # change in diveristy when area is increased by 50%
0.9436*log(1.5)
4.23006*log(1.5)

exp(1/2.58710)

# plotting predictions
x2s = c(-.7,0.4,1.61)
# equivalent to 0.5 hectares, 1.5 hectares, and 5 hectares

cfs <- coef(g3)
intercept <- 6.71 
sm_sl <- 0.3707452
size_sl <- 2.587102
mean(cfs$siteID$`(Intercept)`)
intercept + size_sl*x2s[3]

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
        legend.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"),
        axis.text = element_text(size = 12, family = "Fira Sans"))


# aside... is there the same relationship with number of strips?
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
  ggplot(aes(log(hectares_in_strips), numStrips))+
  geom_text(aes(label = siteID))

t1 <- lm(log(hectares_in_strips) ~ numStrips, site_div_rich)
summary(t1)

g3b <- lmer(gamma_div ~ year + species_seeded + numStrips + 
             (1|siteID), data = site_div_rich)
summary(g3b)


# 2B. ------ Beta diversity models ------------------------
hist(site_div_rich$beta_div)

b_all <- lmer(beta_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID), data = site_div_rich)
summary(b_all)
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
anova(b2, b1) # out!
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

anova(b3)
rand(b3) # random effect matters
performance::check_model(b3)
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
  theme(axis.title = element_text(size = 14, family = "Fira Sans"),
        axis.text = element_text(size = 12, family = "Fira Sans"))
  

# 2C. ------ Alpha diveristy models ----------------------
data("quad_div_rich")
hist(quad_div_rich$alpha_div)

a_all <- lmer(alpha_div ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded + 
                (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
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
confint.merMod(a2) * log(2)
0.710130 * log(2)    # slope estimate for area, getting doubling effect
rand(a2)             # both random effects matter
performance::r2(a2)  # model explains less than gamma/beta diversity models
performance::check_model(a2)

predict(a2, newdata = quad_div_rich)

predict(m, newdata = df, se.fit = T, interval='confidence')

x2s = c(-.7,0.4,1.61)
intercept <- 9.570565
size_sl <- 0.710130
intercept + size_sl*x2s[3]

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
  theme(axis.title = element_text(size = 14, family = "Fira Sans"),
        axis.text = element_text(size = 12, family = "Fira Sans"),
        legend.text = element_text(size = 12, family = "Fira Sans"))

# 3A. ------ Richness of the prairie community ---------------

hist(site_div_rich$p_rich)

p_all <- lmer(p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
anova(p_all)
resid_panel(p_all)  # doesn't look great

site_div_rich <-
  site_div_rich %>%
  mutate(log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

p_log <- lmer(log_p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + season_seeded +
                (1|siteID), site_div_rich)
summary(p_log)
anova(p_log)        # similar p-values to untransformed
resid_panel(p_log)  # better
resid_compare(list(p_all, p_log))
performance::compare_performance(p_all, p_log)

p_poi <- glmer(p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
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

p_nb <- glmer.nb(p_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                   age_yrs + 
                   season_seeded +
                   (1|siteID), site_div_rich)     # also singular fit...
ggResidpanel::resid_panel(p_nb)
performance::check_model(p_nb)

# going to go ahead with log-transformed count data

anova(p_log)

# nix p a ratio

p0 <- lmer(log_p_rich ~ year + species_seeded + log(hectares_in_strips) + age_yrs + 
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
exp(0.016765)   
exp(0.016765*5)
# slope for species_seeded
(exp(0.016765)-1)*100         # percent increase
confint.merMod(p2) %>% exp()  # CI for percent increase

2^0.080320                    # slope for size (multiplicative for doubling size)
2^0.011177323
2^0.14949540

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
  theme(axis.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"),
        legend.text = element_text(size = 12, family = "Fira Sans"))


# 3B. ------ Richness of the weedy community -------------
hist(site_div_rich$w_rich)

w_all <- lmer(w_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
summary(w_all)
ggResidpanel::resid_panel(w_all)

w_poi <- glmer(w_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                 age_yrs + 
                 season_seeded +
                 (1|siteID), site_div_rich, family = poisson)        # model doesn't converge....grr

w_log <- lmer(log_w_rich ~ year + species_seeded + log(hectares_in_strips) + log(avg_p_a) +
                age_yrs + 
                season_seeded +
                (1|siteID), site_div_rich)
ggResidpanel::resid_panel(w_log)
anova(w_log)

# nix age
w1 <- lmer(log_w_rich ~ year + species_seeded + log(hectares_in_strips) +
             log(avg_p_a) +
             season_seeded +
             (1|siteID), site_div_rich)
anova(w_log, w1)    # out!
anova(w1)

# nix season
w2 <- lmer(log_w_rich ~ year + species_seeded + log(hectares_in_strips) +
             log(avg_p_a) +
             (1|siteID), site_div_rich)
anova(w2, w1)       # out!
anova(w2)

# nix p_a ratio
w3 <- lmer(log_w_rich ~ year + species_seeded + log(hectares_in_strips) +
             (1|siteID), site_div_rich)
anova(w3, w2)    # out!
anova(w3)

# nix size
w4 <- lmer(log(w_rich) ~ year + species_seeded + 
             (1|siteID), site_div_rich)
anova(w3, w4)       # out!

# w4 is the final model
anova(w4)
summary(w4)
confint.merMod(w4)
performance::compare_performance(w_log, w1, w2, w3, w4)  # yup w4 is the best
performance::r2(w4)                                  # woof fixed effects explain little variation
performance::check_model(w4)
rand(w4)

slopes <- data.frame(int = c(2.422168, 2.51052, 2.607707),
                     sl  = c( 0.012534,  0.012534,  0.012534),
                     id  = c("0.5 ha", "1.5 ha", "5 ha"))

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
  theme(axis.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"),
        legend.text = element_text(size = 12, family = "Fira Sans"))

# 4A. ------ Alpha prairie richness ----------
quad_div_rich <- 
  quad_div_rich %>%
  mutate(p_rich     = replace(p_rich, p_rich == 0, 0.001),
         w_rich     = replace(w_rich, w_rich == 0, 0.001),
         log_p_rich = log(p_rich),
         log_w_rich = log(w_rich))

ap_all <- lmer(p_rich ~ year + species_seeded + age_yrs + log(hectares_in_strips) + log(avg_p_a) +
                 season_seeded +
                 (1|quadratID:siteID) + (1|siteID), data = quad_div_rich)
summary(ap_all)
anova(ap_all)
resid_panel(ap_all)

# nix p_a ratio

ap0 <- lmer(p_rich ~ year + species_seeded + age_yrs + log(hectares_in_strips) + 
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
confint.merMod(ap2)
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

# 4B. ------ Alpha weedy richness -----------

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

performance::check_model(wp3)
performance::r2(wp3)     # ha, fixed effects explain very little...

# 5.  ------ Mantel test looking at association between seed mix and all spp found ---- 

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




# 6.  ------ Make figures for strips summary slides

# linear model between gamma diversity and seed mix, for 2019 only

l1 <- lm(gamma_div ~ species_seeded, data = filter(site_div_rich, year == "2019"))
summary(l1)
anova(l1)

l2 <- lm(p_rich ~ species_seeded, data = filter(site_div_rich, year == "2019"))
summary(l2)
anova(l2)

g1 <- 
  site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, gamma_div))+
  geom_smooth(method = "lm", color = "#C669D5", fill = "#C669D5", size = 1.5)+
  geom_point(shape = 21, size = 3, color = "black", stroke = 1.5)+
  scale_y_continuous(limits = c(8, 40))+
  labs(x = "Seed mix richness",
       y = "Diversity (e^H')")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))+
  annotate("text", x = 25, y = 38, label = "bold(R^2 == 0.47)", parse = TRUE,
           size = 5)+
  annotate("text", x = 25, y = 34, label = "bold(p < 0.001)", parse = TRUE,
           size = 5)
g1
g2 <- 
  site_div_rich %>%
  filter(year == "2019") %>%
  ggplot(aes(species_seeded, p_rich))+
  geom_smooth(method = "lm", color = "#C669D5", fill = "#C669D5", size = 1.5)+
  geom_point(shape = 21, size = 3, color = "black", stroke = 1.5)+
  scale_y_continuous(limits = c(8, 40))+
  labs(x = "Seed mix richness",
       y = "Prairie species richness")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))+
  annotate("text", x = 25, y = 38, label = "bold(R^2 == 0.44)", parse = TRUE,
           size = 5)+
  annotate("text", x = 25, y = 34, label = "bold(p < 0.001)", parse = TRUE,
           size = 5)
g2
library(patchwork)
g1 + g2
