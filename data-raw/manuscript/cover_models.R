# Analyses regarding the cover of different functional groups 
# The groupings are:
# 1. All target spp
# 2. All weedy spp
# 3. Prairie grasses (and then C3 vs C4 grass)
# 4. Prairie forbs (and then leguminous vs non-leguminous forbs)
# 5. Perennial weeds 
# 6. Annual weeds

# Load libraries and datasets --------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lmerTest)
# remotes::install_github("lydiaPenglish/STRIPSveg")  
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
library(patchwork)

theme_set(theme_bw())

data("prairie_pi")          #  created in "00b_format_veg_cov_data.R"
data("weedy_pi")            #  created in "00b_format_veg_cov_data.R"

# function to plot all the predictor variables against the response to spot any 
# interactions/trends
plot_dat <- function(dat, yy){
  zz <- dat %>%
    select(year, all_of(yy), species_seeded, age_yrs, hectares_in_strips, 
           avg_p_a) %>%
    mutate_at(vars(hectares_in_strips:avg_p_a), ~log(.)) %>%
    tidyr::pivot_longer(cols = species_seeded:avg_p_a, names_to = "variable") %>%
    arrange(year, variable) %>%
    ggplot(aes_string("value", yy))+
    geom_point(aes(color = year ))+
    geom_smooth(aes(color  = year), method = "lm")+
    facet_wrap(~ variable, scales = "free_x")
  ww <- ggplot(dat, aes_string("season_seeded", yy))+
    geom_boxplot(aes(color = year))
  zz + ww + plot_layout(widths = c(3, 1))
}
# ---- 1. all prairie spp --------------------------------------------------

plot_dat(dat = prairie_pi, yy = "prairie_pi_logit") 
# ^ doesn't look like any significant trends 

p0 <- lmer(prairie_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
summary(p0)
anova(p0)

# nix age
p1 <- lmer(prairie_pi_logit ~ year + species_seeded + 
             log(hectares_in_strips) + log(avg_p_a) +season_seeded + 
             (1|siteID), prairie_pi)
anova(p0, p1)    # out! 
anova(p1)

# nix size
p2 <- lmer(prairie_pi_logit ~ year + species_seeded + log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
anova(p1, p2)     # out!
anova(p2)

# nix p-to-a ratio
p3 <- lmer(prairie_pi_logit ~ year + species_seeded + season_seeded + 
             (1|siteID), prairie_pi)
anova(p2, p3)     # out!
anova(p3)

# nix season
p4 <- lmer(prairie_pi_logit ~ year + species_seeded +
             (1|siteID), prairie_pi)
anova(p3, p4)

#final model = p4
summary(p4)
anova(p4)
rand(p4)     # effect matters
performance::r2(p4)
performance::check_model(p4b)
ggResidpanel::resid_panel(p4)
performance::compare_performance(p4, p4b)


# ---- 2. All weedy spp ----
plot_dat(weedy_pi, "weed_pi_logit")

w0 <- lmer(weed_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) + 
             season_seeded + 
             (1|siteID), weedy_pi)
anova(w0)

# nix age
w1 <- lmer(weed_pi_logit ~ year + species_seeded + 
             log(hectares_in_strips) + log(avg_p_a) + 
             season_seeded + 
             (1|siteID), weedy_pi)
anova(w1, w0)   # out!
anova(w1)

# nix size
w2 <- lmer(weed_pi_logit ~ year + species_seeded +  log(avg_p_a) + 
             season_seeded + 
             (1|siteID), weedy_pi)
anova(w2, w1)       # out!
anova(w2)

# nix p-a ratio
w3 <- lmer(weed_pi_logit ~ year + species_seeded + season_seeded +  
             (1|siteID), weedy_pi)
anova(w2, w3)       # out!
anova(w3)

# nix season
w4 <-  lmer(weed_pi_logit ~ year + species_seeded +
              (1|siteID), weedy_pi)
anova(w3, w4)       # out!

# w4 is the final model
summary(w4)
anova(w4)
performance::check_model(w4)
performance::r2(w4)
rand(w4)

# ---- 3. Prairie grass -------------------------------------------------------

# A. all prairie grass

plot_dat(prairie_pi, "pg_pi_logit")
# ^ weak trends with age, speices_seeded

g0 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
summary(g0)
anova(g0)

# nix p-to-a ratio
g1 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips)  + season_seeded + 
             (1|siteID), prairie_pi)
anova(g0, g1)     # out!
anova(g1)

# nix season

g2 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips)  +  
             (1|siteID), prairie_pi)
anova(g2, g1)
anova(g2)

# nix size
g3 <- lmer(pg_pi_logit ~ year + species_seeded + age_yrs + 
             (1|siteID), prairie_pi)
anova(g3, g2)     # out!
anova(g3)

# nix age
g4 <- lmer(pg_pi_logit ~ year + species_seeded + 
             (1|siteID), prairie_pi)
anova(g4, g3)
anova(g4)

# final model is g4
summary(g4)
performance::r2(g4)
rand(g4)
performance::check_model(g4)

# B. C4 grasses

plot_dat(prairie_pi, "c4_pi_logit")

c40 <- lmer(c4_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
              (1|siteID), prairie_pi)
summary(c40)
anova(c40)

# nix p_a ratio
c41 <- lmer(c4_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + season_seeded + 
              (1|siteID), prairie_pi)
anova(c41, c40)     # out!
anova(c41)

# nix season
c42 <- lmer(c4_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + 
              (1|siteID), prairie_pi)
anova(c41, c42)     # out!
anova(c42)

# nix size 
c43 <- lmer(c4_pi ~ year + species_seeded + age_yrs + 
              (1|siteID), prairie_pi)
anova(c43, c42)     # out!
anova(c43)
summary(c43)

# nix age 
c44 <- lmer(c4_pi_logit ~ year + species_seeded + 
              (1|siteID), prairie_pi)
anova(c43, c44)     # out!
anova(c44)

# c44 is final model
rand(c44)
summary(c44)
anova(c44)
performance::r2(c44)
performance::check_model(c44)


# C. C3 grasses
plot_dat(prairie_pi, "c3_pi_logit")

c30 <- lmer(c3_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
              (1|siteID), prairie_pi)
summary(c30)
anova(c30)

# nix age
c31 <- lmer(c3_pi_logit ~ year + species_seeded +
              log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
              (1|siteID), prairie_pi)
anova(c30, c31)   # out!
anova(c31)

# nix season 
c32 <- lmer(c3_pi_logit ~ year + species_seeded +
              log(hectares_in_strips) + log(avg_p_a) + 
              (1|siteID), prairie_pi)
anova(c32, c31)   # out!
anova(c32)

# nix size 
c33 <- lmer(c3_pi_logit ~ year + species_seeded + log(avg_p_a) + 
              (1|siteID), prairie_pi)
anova(c33, c32)  # keep!

# nix edginess
c34 <- lmer(c3_pi_logit ~ year + species_seeded +
              log(hectares_in_strips) +  
              (1|siteID), prairie_pi)
anova(c34, c32)  # keep!

# hmmm now size and edginess are predictors of c3 grasses
# final model is c32

anova(c32)
summary(c32)

# seed mix richness barely significant anymore....
rand(c32)
performance::r2(c32)
performance::check_model(c32)
performance::compare_performance(c30, c31, c32, c33, c34)
confint.merMod(c32)

# ---- 4. Prairie forbs ------------------------------------------------------
plot_dat(prairie_pi, "pf_pi_logit")
# looks like age and seed mix rich are close to being sig?

# A. all forbs
f0 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
anova(f0)

# nix size
f1 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + 
             log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
anova(f0, f1)      # out!
anova(f1)

# nix ratio
f2 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + 
             season_seeded + 
             (1|siteID), prairie_pi)
anova(f2, f1)      # out!
anova(f2)

# nix season
f3 <- lmer(pf_pi_logit ~ year + species_seeded + age_yrs + 
             (1|siteID), prairie_pi)
summary(f3)
anova(f3, f2)      # out! 

# nix age
f4 <- lmer(pf_pi_logit ~ year+species_seeded + 
             (1|siteID), prairie_pi)
anova(f4, f3)     # out!
anova(f4)
summary(f4)

# look at interaction 
f4b <- lmer(pf_pi_logit ~ year*species_seeded + 
              (1|siteID), prairie_pi)
anova(f4, f4b) # don't keep it...

# f4 is the model to use
summary(f4)
rand(f4)
anova(f4)
performance::r2(f4)
performance::check_model(f4)

# B. legumes - significantly go down with age...

plot_dat(prairie_pi, "leg_pi_logit") # actually it looks like legumes have a relationship
# with many of these variables...but definitely the stronges with age

l0 <- lmer(leg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
             (1|siteID), prairie_pi)
anova(l0)
summary(l0)

# nix p_a ratio

l1 <- lmer(leg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + season_seeded + 
             (1|siteID), prairie_pi)
anova(l0, l1)    # out!
anova(l1)

# nix season 
l2 <- lmer(leg_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + 
             (1|siteID), prairie_pi)
anova(l2, l1)    # out!
anova(l2)

# nix size
l3 <- lmer(leg_pi_logit ~ year + species_seeded + age_yrs + 
             (1|siteID), prairie_pi)
anova(l2, l3)    # keep!

# l2 is the final model
summary(l2)
anova(l2)
rand(l2)      # site isn't significant
confint.merMod(l2)
performance::r2(l2)
performance::check_model(l2)
performance::compare_performance(l0, l1, l2)

l2_log <- lmer(log(leg_pi) ~ year + species_seeded + age_yrs + 
                 log(hectares_in_strips) + 
                 (1|siteID), prairie_pi)
summary(l2_log)



# C. non-leguminous forbs
plot_dat(prairie_pi, "nl_pi_logit")

nl0 <- lmer(nl_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + log(avg_p_a) + season_seeded + 
              (1|siteID), prairie_pi)
summary(nl0)
anova(nl0)

# nix size 
nl1 <- lmer(nl_pi_logit ~ year + species_seeded + age_yrs + 
              log(avg_p_a) + season_seeded + 
              (1|siteID), prairie_pi)
anova(nl1, nl0)
anova(nl1)     # out!

# nix p_a_ratio
nl2 <- lmer(nl_pi_logit ~ year + species_seeded + age_yrs + 
              season_seeded + 
              (1|siteID), prairie_pi)
anova(nl1, nl2)      # out!
anova(nl2)

# nix age
nl3 <- lmer(nl_pi_logit ~ year + species_seeded +  
              season_seeded + 
              (1|siteID), prairie_pi)
anova(nl3, nl2)     # out!
anova(nl3)

# nix season 
nl4 <- lmer(nl_pi_logit ~ year + species_seeded +  
              (1|siteID), prairie_pi)
anova(nl3, nl4)     # out!
anova(nl4)

# double check interaction between year
nl4b <- lmer(nl_pi_logit ~ year*species_seeded +  
               (1|siteID), prairie_pi)
anova(nl4b) # nope! 

# nothing sig, nl4 is the final model
summary(nl4)
anova(nl4)
rand(nl4)
performance::r2(nl4)
performance::check_model(nl4)

nl4_log <- lmer(nl_pi_logit ~ year*species_seeded +  
                  (1|siteID), prairie_pi)
summary(nl4_log)
anova(nl4_log)

nl4_log2 <- lmer(nl_pi_logit ~ year+species_seeded +  
                   (1|siteID), prairie_pi)

anova(nl4_log, nl4_log2) # interaction ns. 

# ---- 5. Perennial weeds ----
# both ok diagnostically, but logit fits better
plot_dat(weedy_pi, "wp_pi_logit")  # lots of noise but also expected trends

wp0 <- lmer(wp_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + log(avg_p_a) +
              season_seeded + 
              (1|siteID), weedy_pi)
anova(wp0)
performance::check_model(wp0)

# nix age
wp1 <- lmer(wp_pi_logit ~ year + species_seeded + 
              log(hectares_in_strips) + log(avg_p_a) +
              season_seeded + 
              (1|siteID), weedy_pi)
anova(wp1, wp0)           # out!
anova(wp1)

# nix size
wp2 <- lmer(wp_pi_logit ~ year + species_seeded +  log(avg_p_a) +
              season_seeded + 
              (1|siteID), weedy_pi)
anova(wp2, wp1)           # out!
anova(wp2)

# nix season
wp3 <- lmer(wp_pi_logit ~ year + species_seeded +  log(avg_p_a) +
              (1|siteID), weedy_pi)
anova(wp2, wp3)           # out!
anova(wp3)

# nix p-to-a ratio
wp4 <- lmer(wp_pi_logit ~ year+species_seeded +
              (1|siteID), weedy_pi)
anova(wp4)
anova(wp3, wp4)           # out!

# wp4 is the final model
summary(wp4)
anova(wp4)
performance::check_model(wp4)
performance::r2(wp4)
rand(wp4)

# adding some terms back to double check...too much noise for any trends/interactions 
wp4b <- lmer(wp_pi_logit ~ year * species_seeded +
               (1|siteID), weedy_pi) # interaction? naw...
anova(wp4b)

# age with interaction - naw
wp4c <- lmer(wp_pi_logit ~ year * age_yrs + species_seeded +
               (1|siteID), weedy_pi)
anova(wp4c)

# p:a rat with interaction - naw
wp4d <- lmer(wp_pi_logit ~ year * log(avg_p_a) + species_seeded +
               (1|siteID), weedy_pi)
anova(wp4d)

# size with interaction - naw...
wp4e <- lmer(wp_pi_logit ~ year * log(hectares_in_strips) + species_seeded +
               (1|siteID), weedy_pi)
anova(wp4e)

# ---- 6. Annual weeds ----
plot_dat(weedy_pi, "wa_pi_logit")

wa0<- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + 
             log(hectares_in_strips) + log(avg_p_a) +
             season_seeded + 
             (1|siteID), weedy_pi)
anova(wa0)

# nix p_a_ratio
wa1 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + 
              log(hectares_in_strips) + 
              season_seeded + 
              (1|siteID), weedy_pi)
anova(wa0, wa1)       # out! 
anova(wa1)

# nix size

wa2 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + season_seeded + 
              (1|siteID), weedy_pi)
anova(wa2, wa1)            # out!
anova(wa2)

# nix season
wa3 <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs  +
              (1|siteID), weedy_pi)
anova(wa3, wa2)            # out!
anova(wa3)

# interaction between age and year
wa3b <- lmer(wa_pi_logit ~ year + species_seeded + age_yrs + year:age_yrs +
               (1|siteID), weedy_pi)
anova(wa3b) # should be including this...

# nix age
wa4 <- lmer(wa_pi_logit ~ year + species_seeded +
              (1|siteID), weedy_pi)
anova(wa4, wa3)            # keep!

# final model is wa3
summary(wa3)
summary(wa3b)
anova(wa3b)
confint.merMod(wa3)
performance::compare_performance(wa0, wa1, wa2, wa3, wa4, wa3b)

# age slope = -0.36402
exp(-0.36402)

summary(wa3)
performance::check_model(wa3)
performance::r2(wa3)
rand(wa3)                  # site doesn't matter...

weedy_pi %>%
  ggplot(aes(age_yrs, log(wa_pi)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~year)

# ---- investigating relationship between age and legume cover more ----

# is the sig relationship between relative cover of legumes and age an artifact
# of less legumes being seeded?

# 1. relationship between age and the number of legumes spp seeded

seed_rich <- readr::read_csv("data-raw/seed_mix_info/all_site_seed_list.csv")

leg_seed_rich <- 
  seed_rich %>%
  left_join(species_list, by = "speciesID") %>%
  filter(family == "fabaceae") %>%
  group_by(siteID) %>%
  summarize(seeded_leg = n()) %>%
  left_join(all_site_info, by = "siteID")

leg_seed_rich %>%
  ggplot(aes(age_yrs, seeded_leg))+
  geom_point()+
  geom_smooth(method = "lm")

leg1 <- lm(log(seeded_leg) ~ age_yrs, leg_seed_rich)
summary(leg1)
performance::check_model(leg1)

# this is probably more appropriate as a glm...
legp1 <- glm(seeded_leg ~ age_yrs, family = poisson, data = leg_seed_rich)
summary(legp1)
performance::check_model(legp1)
performance::check_overdispersion(legp1)

# 2. relationship between age and relative amount of seed mix devoted to legumes
# (only a subset of sites)

seed_div <- readr::read_csv("data-raw/seed_mix_info/all_site_seed_div.csv")

leg_PLS_pi <- seed_div %>%
  group_by(siteID) %>%
  mutate(tot_PLS = sum (PLS_lbs_acre),
         pi_PLS  = PLS_lbs_acre/tot_PLS) %>%
  left_join(species_list, by = "speciesID") %>%
  filter(family == "fabaceae") %>%
  summarize(leg_PLS_pi = sum(pi_PLS)) %>%
  left_join(all_site_info, by = "siteID")

leg_PLS_pi %>%
  ggplot(aes(age_yrs, log(leg_PLS_pi)))+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, color = "#361554", size = 1, lty = 2)+
  labs(x = "Site age (yrs)",
       y = "Legume PLS proportion\n (log)")+
  theme(axis.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"))

leg_PLS_pi %>%
  ggplot(aes(age_yrs, leg_PLS_pi))+
  geom_point(size = 2)+
  geom_abline(slope = -0.5627, intercept = 1.0519, size = 1,
              lty = 2, color = "#361554")+
  geom_text(aes(7, 0.45), label = "R^2 == 0.56", parse = TRUE, 
            family = "Fira Sans")+
  geom_text(aes(7, 0.35), label = "p < 0.001", parse = TRUE,
            family = "Fira Sans")+
  geom_text(aes(7, 0.3), label = "n == 15", parse = TRUE,
            family = "Fira Sans")+
  scale_y_continuous(trans = "log", breaks = c(0.05, 0.1, 0.25, 0.5))+
  labs(x = "Site Age (yrs)",
       y = "Legume PLS proportion")+
  theme(axis.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"))


leg2 <- lm(car::logit(leg_PLS_pi) ~ age_yrs, leg_PLS_pi)
summary(leg2)
performance::check_model(leg2)

leg3 <- lm(log(leg_PLS_pi) ~ age_yrs, leg_PLS_pi)
summary(leg3)
ggResidpanel::resid_panel(leg3)

# yes, negative relationship between relative proportion of seed mix in legume seeds 
# and the age of sites

# ---- investigating relationship between size/edigness and C3 cover more ---------

# recall model:
c32 <- lmer(c3_pi_logit ~ year + species_seeded +
              log(hectares_in_strips) + log(avg_p_a) + 
              (1|siteID), prairie_pi)
summary(c32)
anova(c32)

seed_div <- readr::read_csv("data-raw/seed_mix_info/all_site_seed_div.csv")

C3_seeds <- seed_div %>%
  group_by(siteID) %>%
  mutate(tot_PLS = sum (PLS_lbs_acre)) %>%
  left_join(species_list, by = "speciesID") %>%
  filter(group == "prairie C3 grass") %>%
  mutate(tot_C3_pls = sum(PLS_lbs_acre),
         prop_c3 = tot_C3_pls/tot_PLS,
         num_spp = n()) %>%
  select(siteID, tot_PLS, tot_C3_pls, prop_c3, num_spp) %>%
  distinct() %>%
  left_join(prairie_pi)

# plotting & testing

# 1. size - doesn't look very related...
C3_seeds %>%
  ggplot(aes(log(hectares_in_strips), car::logit(prop_c3)))+
  geom_point()+
  geom_smooth(method = "lm")

C3_seeds %>%
  ggplot(aes(log(hectares_in_strips), log(tot_C3_pls)))+
  geom_point()+
  geom_smooth(method = "lm")

# 1. p-to-a ratio - again doesn't look very related

C3_seeds %>%
  ggplot(aes(log(avg_p_a), car::logit(prop_c3)))+
  geom_point()+
  geom_smooth(method = "lm")

C3_seeds %>%
  ggplot(aes(log(avg_p_a), log(tot_C3_pls)))+
  geom_point()+
  geom_smooth(method = "lm")