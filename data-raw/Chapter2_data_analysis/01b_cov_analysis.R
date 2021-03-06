# Analysis regarding the cover of different functional groups - uses dataframes created in 
# "00b_format_veg_cov_data.R"

library(lmerTest)
library(tidyverse)
library(STRIPS2veg)
library(ggResidpanel)
library(performance)
library(patchwork)
library(extrafont)
theme_set(theme_bw())

data("prairie_pi")
data("weedy_pi")

# backtranform logit estimates to probabilites
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

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

# I will model the relative cover of ...
# 1. All prairie spp
# 2. Prairie grasses (and then C3 vs C4 grass)
# 3. Prairie forbs (and then leguminous vs non-leguminous forbs)
# 4. All weedy spp
# 5. Perennial weeds 
# 6. Annual weeds

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


# ---- 2. Prairie grass -------------------------------------------------------

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
confint.merMod(c32)

# ---- 3. Prairie forbs ------------------------------------------------------
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

# ---- 4. All weedy spp ----
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
summary(wa3b)
anova(wa3b)
confint.merMod(wa3)

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

# ---- prairie vs weedy cover/richness ----
data("site_div_rich")

# prairie cover vs weedy richness
pra_vs_wd <- left_join(site_div_rich, prairie_pi)

df <- data.frame(year = c("2018", "2019"),
                 ints = c(3.73743, 3.85857),
                 slps = c(-1.36014, -1.36014))

pra_cov <- 
  ggplot(pra_vs_wd, aes(prairie_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df, aes( 
              intercept = ints, slope = slps,
              color = year), lty = 2, size = 1)+
  geom_text(aes(0.75, 43), label = "R[m]^2 == 0.48", parse = TRUE,
            family = "Fira Sans")+
  geom_text(aes(0.75, 39), label = "p[year] == 0.04", parse = TRUE,
            family = "Fira Sans")+
  geom_text(aes(0.75, 36), label = "p[cov] < 0.001", parse = TRUE,
            family = "Fira Sans")+
  labs(x = "Relative Cover",
       y = "Weed Species Richness",
       color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(8, 45), breaks = c(15, 30, 45))+
  ggtitle("A. All Prairie")+
  theme(axis.text  = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"),
        legend.title = element_text(size = 14, family = "Fira Sans"),
        legend.text = element_text(size = 12, family = "Fira Sans"),
        plot.title = element_text(size = 15, family = "Fira Sans"))
pra_cov

pra_cov_log <- 
  ggplot(pra_vs_wd, aes(prairie_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df, aes( 
    intercept = ints, slope = slps,
    color = year), lty = 2, size = 1)+
  geom_text(aes(0.75, 43), label = "R[m]^2 == 0.49", parse = TRUE)+
  geom_text(aes(0.75, 39), label = "p[year] == 0.04", parse = TRUE)+
  geom_text(aes(0.75, 36), label = "p[cov] < 0.001", parse = TRUE)+
  labs(x = "Relative Cover",
       y = "Weed Species Richness",
       color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(8, 45), breaks = c(15, 30, 45))+
  ggtitle("A. All Prairie")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
pra_cov_log

pw1 <- lmer(log(w_rich) ~ year + prairie_pi + (1|siteID), pra_vs_wd)
pw2 <- lmer(log(w_rich) ~ year*prairie_pi + (1|siteID), pra_vs_wd)
anova(pw1, pw2) # no interaction
summary(pw1)
anova(pw1)
performance::check_model(pw1)
performance::r2(pw1)

# weedy richness vs prairie grass cov - SIG

df2 <- data.frame(year = c("2018", "2019"),
                  ints = c( 3.20797, 3.20797+0.14252),
                  slps = c(-1.09050, -1.09050))

pg_cov <- 
  ggplot(pra_vs_wd, aes(pg_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df2, aes( 
    intercept = ints, slope = slps,
    color = year), lty = 2, size = 1)+
  geom_text(aes(0.6, 43), label = "R[m]^2 == 0.32", parse = TRUE)+
  geom_text(aes(0.6, 39), label = "p[year] == 0.03", parse = TRUE)+
  geom_text(aes(0.6, 36), label = "p[cov] == 0.001", parse = TRUE)+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(8, 45), breaks = c(15, 30, 45))+
  ggtitle("B. Grasses")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
pg_cov

gw1 <- lmer(log(w_rich) ~ year+pg_pi + (1|siteID), pra_vs_wd)
gw2 <- lmer(log(w_rich) ~ year*pg_pi + (1|siteID), pra_vs_wd)
anova(gw1, gw2)  # no interaction
summary(gw1)
anova(gw1)
performance::r2(gw1)
performance::check_model(gw1)

# weedy richness vs prairie forb cov

df3 <- data.frame(year = c("2018", "2019"),
                  ints = c(2.85937, 2.85937+0.50972),
                  slps = c(0.01535, -1.04573))

pf_cov <- 
  ggplot(pra_vs_wd, aes(pf_pi, w_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df3, aes( 
          intercept = ints, slope = slps,
          color = year), lty = 2, size = 1)+
 # geom_smooth(aes(color  = year), method = "lm", se = FALSE, lty = 2)+
  geom_text(aes(0.45, 43), label = "R[m]^2 == 0.14", parse = TRUE)+
  geom_text(aes(0.45, 39), label = "p[year] == 0.002", parse = TRUE)+
  geom_text(aes(0.45, 36), label = "p[cov] == 0.25", parse = TRUE)+
  geom_text(aes(0.45, 33), label = "p[year*cov] == 0.02", parse = TRUE)+
  labs(x = "Relative Cover",
       y = NULL, color = "Year sampled",
       fill = "Year sampled")+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(8, 45), breaks = c(15, 30, 45))+
  ggtitle("C. Forbs")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
pf_cov
library(patchwork)
pra_cov_log + pg_cov + pf_cov + plot_layout(guides = 'collect')

fw1 <- lmer(log(w_rich) ~ year*pf_pi + (1|siteID), pra_vs_wd)
summary(fw1)
anova(fw1)
performance::check_model(fw1)
performance::r2(fw1)

# weed pi vs. richness of prairie species
wd_vs_pra <- left_join(site_div_rich, weedy_pi)

df4 <- data.frame(year = c("2018", "2019"),
                  ints = c(3.24488, 3.24488 + 0.01373),
                  slps = c(-0.58328, -0.58328))
w_cov <- 
  ggplot(wd_vs_pra, aes(weed_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df4, aes( 
    intercept = ints, slope = slps,
    color = year), lty = 2, size = 1)+
  #geom_smooth(method = "lm", se = FALSE, lty = 2, color = "black")+
  geom_text(aes(0.7, 43), label = "R[m]^2 == 0.16", parse = TRUE)+
  geom_text(aes(0.7, 39), label = "p[year] == 0.80", parse = TRUE)+
  geom_text(aes(0.7, 36), label = "p[cov] == 0.02", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(10, 45), breaks = c(15, 30, 45))+
  ggtitle("A. All Weeds")+
  labs(x = "Relative Cover",
       y = "Prairie Species Richness", 
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
w_cov
wp1 <- lmer(log(p_rich) ~ year + weed_pi + (1|siteID), wd_vs_pra)
summary(wp1)
anova(wp1)
performance::check_model(wp1)
performance::r2(wp1)

# annual weed pi vs prairie richness - NS

df5 <- data.frame(year = c("2018", "2019"),
                  ints = c(3.08003, 3.0800-0.00972),
                  slps = c(-0.31283, -0.31283))

aw_cov <- 
  ggplot(wd_vs_pra, aes(wa_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df5, aes( 
    intercept = ints, slope = slps,
    color = year), lty = 2, size = 1)+
  #geom_smooth(method = "lm", se = FALSE, lty = 2, color = "black")+
  geom_text(aes(0.4, 43), label = "R[m]^2 == 0.01", parse = TRUE)+
  geom_text(aes(0.4, 39), label = "p[year] == 0.85", parse = TRUE)+
  geom_text(aes(0.4, 36), label = "p[cov] == 0.43", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(10, 45), breaks = c(15, 30, 45))+
  ggtitle("B. Annual Weeds")+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
aw_cov

ap1 <- lmer(log(p_rich) ~ year + wa_pi + (1|siteID), wd_vs_pra)
summary(ap1)
anova(ap1)
performance::check_model(ap1)
performance::r2(ap1)

# perennial weedy pi vs. prairie richness - nearly SIG
df6 <- data.frame(year = c("2018", "2019"),
                  ints = c(3.22157,  3.22157 + 0.01941),
                  slps = c(-0.73328, -0.73328))

pw_cov <- 
  ggplot(wd_vs_pra, aes(wp_pi, p_rich))+
  geom_point(aes(fill = year), size = 3, pch = 21)+
  geom_abline(data = df6, aes( 
    intercept = ints, slope = slps,
    color = year), lty = 2, size = 1)+
  #geom_smooth(method = "lm", se = FALSE, lty = 2, color = "black")+
  geom_text(aes(0.45, 43), label = "R[m]^2 == 0.15", parse = TRUE)+
  geom_text(aes(0.45, 39), label = "p[year] == 0.72", parse = TRUE)+
  geom_text(aes(0.45, 36), label = "p[cov] == 0.02", parse = TRUE)+
  scale_color_grey(start = 0.3, end = 0.6)+
  scale_fill_grey(start = 0.3, end = 0.6)+
  scale_y_continuous(trans = "log", limits = c(10, 45), breaks = c(15, 30, 45))+
  ggtitle("C. Perennial Weeds")+
  labs(x = "Relative Cover",
       y = NULL,
       color = "Year sampled",
       fill = "Year sampled")+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15))
pw_cov

pp1 <- lmer(log(p_rich) ~ year + wp_pi + (1|siteID), wd_vs_pra)
summary(pp1)
anova(pp1)
performance::check_model(pp1)
performance::r2(pp1)
rand(pp1)


w_cov + aw_cov + pw_cov + plot_layout(guides = 'collect')

# prairie cov vs. weed cov

all_cov <- left_join(prairie_pi, weedy_pi)

all_cov %>%
  ggplot(aes(weed_cov, prairie_cov))+
  geom_point(aes(color = year)) +
  geom_smooth(aes(color = year), method = "lm", se = FALSE)

wpcov <- lmer(prairie_cov ~ year+weed_cov + (1|siteID), all_cov)
summary(wpcov)
anova(wpcov)

# on alpha level
pc <- veg_mid %>%
  select(year, quadratID, siteID, one_of(nat_codes)) %>%
  mutate(nat_cov = rowSums(.[, 4:ncol(.)])) %>%
  select(year, quadratID, siteID, nat_cov)
wc <- veg_mid %>%
  select(year, quadratID, siteID, one_of(weeds)) %>%
  mutate(wd_cov = rowSums(.[, 4:ncol(.)])) %>%
  select(year, quadratID, siteID, wd_cov)

alpha_cov <- left_join(pc, wc)

ac <- lmer(nat_cov ~ year + wd_cov + (1|quadratID:siteID) + (1|siteID), alpha_cov)
summary(ac)
anova(ac)

alpha_cov %>%
  ggplot(aes(wd_cov, nat_cov))+
  geom_point(aes(color = year))+
  geom_smooth(aes(color = year), method = "lm", se = FALSE) +
  facet_wrap(~siteID)

# prairie richness vs weed richness

pra_vs_wd %>%
  ggplot(aes(w_rich, p_rich))+
  geom_point(aes(color = year))+
  geom_smooth(aes(color = year), method = "lm", se = FALSE)

pw1 <- lmer(p_rich ~ year+w_rich + (1|siteID), pra_vs_wd)
summary(pw1)
anova(pw1)

# no relationship between prairie and weedy richness (i.e. can have lots of prairie
# and weedy plants coexisting)

# ---- investigating relationship between age and legume cover more ----

# is the sig relationship between relative cover of legumes and age an artifact
# of less legumes being seeded?

# 1. relationship between age and the number of legumes spp seeded

seed_rich <- read_csv("data-raw/seed_mix_info/all_site_seed_list.csv")

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

seed_div <- read_csv("data-raw/seed_mix_info/all_site_seed_div.csv")

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

seed_div <- read_csv("data-raw/seed_mix_info/all_site_seed_div.csv")

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
