library(lmerTest)
library(tidyverse)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
theme_set(theme_bw())

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

# prairie cover vs weedy richness
pra_vs_wd <- left_join(site_div_rich, prairie_pi)

pra_cov <- 
  ggplot(pra_vs_wd, aes(prairie_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color  = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.75, 38), label = "R[m]^2 == 0.48", parse = TRUE)+
  geom_text(aes(0.75, 36.5), label = "p[year] == 0.04", parse = TRUE)+
  geom_text(aes(0.75, 35), label = "p[cov] < 0.001", parse = TRUE)+
  labs(x = "Relative Cover",
       y = "Weed Species Richness",
       color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("A. All Prairie")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14))
pra_cov

pw1 <- lmer(log(w_rich) ~ year + prairie_pi + (1|siteID), pra_vs_wd)
summary(pw1)
anova(pw1)
performance::check_model(pw1)
performance::r2(pw1)

# weedy richness vs prairie grass cov - SIG

pg_cov <- 
  ggplot(pra_vs_wd, aes(pg_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color  = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.6, 38), label = "R[m]^2 == 0.31", parse = TRUE)+
  geom_text(aes(0.6, 36.5), label = "p[year] == 0.02", parse = TRUE)+
  geom_text(aes(0.6, 35), label = "p[cov] == 0.002", parse = TRUE)+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("B. Grasses")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title  = element_text(size = 14))

gw1 <- lmer(log(w_rich) ~ year + pg_pi + (1|siteID), pra_vs_wd)
summary(gw1)
anova(gw1)
performance::r2(gw1)
performance::check_model(gw1)

# weedy richness vs prairie forb cov

pf_cov <- 
  ggplot(pra_vs_wd, aes(pf_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color  = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.45, 38), label = "R[m]^2 == 0.14", parse = TRUE)+
  geom_text(aes(0.45, 36.5), label = "p[year] == 0.002", parse = TRUE)+
  geom_text(aes(0.45, 35), label = "p[cov] == 0.29", parse = TRUE)+
  geom_text(aes(0.45, 33.5), label = "p[year*cov] == 0.02", parse = TRUE)+
  labs(x = "Relative Cover",
       y = NULL, color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("C. Forbs")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title  = element_text(size = 14))
pf_cov
library(patchwork)
pra_cov + pg_cov + pf_cov + plot_layout(guides = 'collect')

fw1 <- lmer(log(w_rich) ~ year*pf_pi + (1|siteID), pra_vs_wd)
summary(fw1)
anova(fw1)
performance::check_model(fw1)
performance::r2(fw1)

# weed pi vs. richness of prairie species
wd_vs_pra <- left_join(site_div_rich, weedy_pi)

w_cov <- 
  ggplot(wd_vs_pra, aes(weed_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.7, 35), label = "R[m]^2 == 0.13", parse = TRUE)+
  geom_text(aes(0.7, 33.5), label = "p[year] == 0.92", parse = TRUE)+
  geom_text(aes(0.7, 32), label = "p[cov] == 0.04", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("A. All Weeds")+
  labs(x = "Relative Cover",
       y = "Prairie Species Richness", 
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14))

wp1 <- lmer(log(p_rich) ~ year + weed_pi + (1|siteID), wd_vs_pra)
summary(wp1)
anova(wp1)
performance::check_model(wp1)
performance::r2(wp1)

# annual weed pi vs prairie richness - NS

aw_cov <- 
  ggplot(wd_vs_pra, aes(wa_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.4, 35), label = "R[m]^2 == 0.02", parse = TRUE)+
  geom_text(aes(0.4, 33.5), label = "p[year] == 0.76", parse = TRUE)+
  geom_text(aes(0.4, 32), label = "p[cov] == 0.36", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("B. Annual Weeds")+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title  = element_text(size = 14))

ap1 <- lmer(log(p_rich) ~ year + wa_pi + (1|siteID), wd_vs_pra)
summary(ap1)
anova(ap1)
performance::check_model(ap1)
performance::r2(ap1)

# perennial weedy pi vs. prairie richness - nearly SIG

pw_cov <- 
  ggplot(wd_vs_pra, aes(wp_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_smooth(aes(color = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.45, 35), label = "R[m]^2 == 0.11", parse = TRUE)+
  geom_text(aes(0.45, 33.5), label = "p[year] == 0.86", parse = TRUE)+
  geom_text(aes(0.45, 32), label = "p[cov] == 0.05", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  ggtitle("C. Perennial Weeds")+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title  = element_text(size = 14))

pp1 <- lmer(log(p_rich) ~ year + wp_pi + (1|siteID), wd_vs_pra)
summary(pp1)
anova(pp1)
performance::check_model(pp1)
performance::r2(pp1)
rand(pp1)


w_cov + aw_cov + pw_cov + plot_layout(guides = 'collect')
