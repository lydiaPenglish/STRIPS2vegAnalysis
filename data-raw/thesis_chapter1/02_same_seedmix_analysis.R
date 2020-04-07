# Case study - sites that had the same seed mix planted #
# Statewide Mesic 10-30 Iowa Pollinator Mix Allendan
# ARM, WOR, MRS, NYK, SER, RHO

library(STRIPS2veg)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(patchwork)
sites <- c("ARM", "WOR", "MRS", "NYK", "SER", "RHO")
data("site_div_rich")
data("quad_div_rich")
source("data-raw/00b_format_veg_cov_data.R")
theme_set(theme_bw())
grey_cols <- c("#373737", "#555555", "#D2D2D2")

# subsets of dataframes..
sub_div_g <- site_div_rich %>%
  filter(siteID %in% sites & year == "2019")
sub_div_q <- quad_div_rich %>%
  filter(siteID %in% sites & year == "2019") %>%
  mutate(p_rich = replace(p_rich, p_rich == 0, 0.01),
         w_rich = replace(w_rich, w_rich == 0, 0.01))
sub_cov_p <- prairie_pi %>%
  filter(siteID %in% sites & year == "2019") %>%
  ungroup()
sub_cov_w <- weedy_pi %>%
  filter(siteID %in% sites & year == "2019") %>%
  ungroup()

# ---- Diversity - wouldn't expect it to differ given same seed mix ----

# 1. gamma diversity
sub_div_g %>%
  ggplot(aes(siteID, gamma_div))+
  geom_col()

#       size - NS
g1 <- lm(gamma_div ~ log(acres_in_strips), sub_div_g)
anova(g1)

#       age - NS
g2 <- lm(gamma_div ~ age_yrs, sub_div_g)
anova(g2)

#       season - NS
g3 <- lm(gamma_div ~ season_seeded, sub_div_g)
anova(g3)

#       perim:area - NS
g4 <- lm(gamma_div ~ avg_p_a, sub_div_g)
anova(g4)


# 2. beta diversity 
sub_div_g %>%
  ggplot(aes(siteID, beta_div))+
  geom_col()

#        size - NS
b1 <- lm(beta_div ~ log(acres_in_strips), sub_div_g)
anova(b1)

#        age - NS
b2 <- lm(beta_div ~ age_yrs, sub_div_g)
anova(b2)

#        season - NS
b3 <- lm(beta_div ~ season_seeded, sub_div_g)
anova(b3)

#       perim:area - NS
b4 <- lm(beta_div ~ avg_p_a, sub_div_g)
anova(b4)


# 3. alpha diversity 
sub_div_q %>%
  ggplot(aes(siteID, alpha_div))+
  geom_boxplot()

#        size - NS
a1 <- lmer(alpha_div ~ log(acres_in_strips) + (1|siteID), sub_div_q)
summary(a1)
anova(a1)

#        age - NS
a2 <- lmer(alpha_div ~ age_yrs + (1|siteID), sub_div_q)
anova(a2)

#        season - NS
a3 <- lmer(alpha_div ~ season_seeded + (1|siteID), sub_div_q)
anova(a3)

#       perim:area - NS
a4 <- lmer(alpha_div ~ avg_p_a + (1|siteID), sub_div_q)
anova(a4)


# ---- Richness of prairie and weedy spp, prairie species richness lowest in summer ----

# 1. Gamma prairie richness
sub_div_g %>%
  ggplot(aes(siteID, log(p_rich)))+
  geom_col()

#        size - NS
p1 <- lm(log(p_rich) ~ log(acres_in_strips), sub_div_g)
summary(p1)

#        age - NS
p2 <- lm(log(p_rich) ~ age_yrs,  sub_div_g)
summary(p2)

#        season - sig!!!
p3 <- lm(log(p_rich) ~ season_seeded, sub_div_g)
summary(p3)
anova(p3)
p3m <- emmeans(p3, ~season_seeded)
pairs(p3m)                         # sig difference between fall and summer
contrast(p3m)
ggResidpanel::resid_panel(p3)

#       perim:area - NS
p4 <- lm(log(p_rich) ~ avg_p_a, sub_div_g)
anova(p4)


# 2. Gamma weedy richness
sub_div_g %>%
  ggplot(aes(siteID, log(w_rich)))+
  geom_col()

#        size - NS
w1 <- lm(log(w_rich) ~ log(acres_in_strips), sub_div_g)
summary(w1)

#        age - NS
w2 <- lm(log(w_rich) ~ age_yrs, sub_div_g)
summary(w2)

#        season - NS
w3 <- lm(log(w_rich) ~ season_seeded, sub_div_g)
summary(w3)
anova(w3)

#       perim:area - NS
w4 <- lm(log(w_rich) ~ avg_p_a, sub_div_g)
anova(w4)
 
# ---- Cover of different functional groups ----

# 1. All prairie, season matters
sub_cov_p %>%
  ggplot(aes(siteID, prairie_pi))+
  geom_col()

#         size - NS
pp1 <- lm(prairie_pi_logit ~ log(acres_in_strips), sub_cov_p)
summary(pp1)

#         age - NS
pp2 <- lm(prairie_pi_logit ~ age_yrs, sub_cov_p)
summary(pp2)

#         season - sig!
pp3 <- lm(prairie_pi_logit ~ season_seeded, sub_cov_p)
summary(pp3)
anova(pp3)
pp3_m <- emmeans(pp3, ~season_seeded)
pairs(pp3_m)
ggResidpanel::resid_panel(pp3)

#       perim:area - NS
pp4 <- lm(prairie_pi_logit ~ avg_p_a, sub_cov_p)
anova(pp4)


# 2. Prairie grass - NS difference
sub_cov_p %>%
  ggplot(aes(siteID, pg_pi))+
  geom_col()

#          size - NS
pg1 <- lm(pg_pi_logit ~ log(acres_in_strips), sub_cov_p)
summary(pg1)

#          age - NS
pg2 <- lm(pg_pi_logit ~ age_yrs, sub_cov_p)
summary(pg2)

#          season - NS
pg3 <- lm(pg_pi_logit ~ season_seeded, sub_cov_p)
summary(pg3)
anova(pg3)

#       perim:area - NS
pg4 <- lm(pg_pi_logit ~ avg_p_a, sub_cov_p)
anova(pg4)

# 3. Prairie forb - season matters for all three
sub_cov_p %>%
  ggplot(aes(siteID, pf_pi))+
  geom_col()

#          size - NS
pf1 <- lm(pf_pi_logit ~ log(acres_in_strips), sub_cov_p)
summary(pf1)

#          age - NS
pf2 <- lm(pf_pi_logit ~ age_yrs, sub_cov_p)
summary(pf2)

#          season - sig for everything
pf3 <- lm(pf_pi_logit ~ season_seeded, sub_cov_p)
summary(pf3)
anova(pf3)
pf3_m <- emmeans(pf3, ~ season_seeded)
pairs(pf3_m)
contrast(pf3_m)
ggResidpanel::resid_panel(pf3)

#       perim:area - NS
pf4 <- lm(pf_pi_logit ~ avg_p_a, sub_cov_p)
anova(pf4)

# 4. All weeds
sub_cov_w %>%
  ggplot(aes(siteID, weed_pi))+
  geom_col()

#         size - NS
ww1 <- lm(weed_pi_logit ~ log(acres_in_strips), sub_cov_w)
summary(ww1)

#         age - NS
ww2 <- lm(weed_pi_logit ~ age_yrs, sub_cov_w)
summary(ww2)

#         season - sig!!
ww3 <- lm(weed_pi_logit ~ season_seeded, sub_cov_w)
summary(ww3)
anova(ww3)
ww3_m <- emmeans(ww3, ~season_seeded)
pairs(ww3_m)        # fall and spring the same, summer different from both

#       perim:area - NS
ww4 <- lm(weed_pi_logit ~ avg_p_a, sub_cov_w)
anova(ww4)

# 5. Annual weeds - NS for all 
sub_cov_w %>%
  ggplot(aes(siteID, wa_pi_logit))+
  geom_col()

#         size - NS
wa1 <- lm(wa_pi_logit ~ log(acres_in_strips), sub_cov_w)
summary(wa1)

#         age - NS
wa2 <- lm(wa_pi_logit ~ age_yrs, sub_cov_w)
summary(wa2)

#         season - NS
wa3 <- lm(wa_pi_logit ~ season_seeded, sub_cov_w)
summary(wa3)

#       perim:area - NS
wa4 <- lm(wa_pi_logit ~ avg_p_a, sub_cov_w)
anova(wa4)

# 6. Perennial weeds
sub_cov_w %>%
  ggplot(aes(siteID, wp_pi))+
  geom_col()

#         size - NS
wp1 <- lm(wp_pi_logit ~ log(acres_in_strips), sub_cov_w)
summary(wp1)

#         age - NS
wp2 <- lm(wp_pi_logit ~ age_yrs, sub_cov_w)
summary(wp2)

#         season
wp3 <- lm(wp_pi_logit ~ season_seeded, sub_cov_w)
summary(wp3)
anova(wp3)

#       perim:area - NS
wp4 <- lm(wp_pi_logit ~ avg_p_a, sub_cov_w)
anova(wp4)

# ---- Manuscript figs ----

gg_wp_rich <- 
  sub_div_g %>%
  select(p_rich, w_rich, season_seeded) %>%
  group_by(season_seeded) %>%
  summarize(avg_p_rich = mean(p_rich),
            sd_p_rich  = sd(p_rich),
            se_p_rich  = sd_p_rich/sqrt(2),
            avg_w_rich = mean(w_rich),
            sd_w_rich  = sd(w_rich),
            se_w_rich  = sd_w_rich/sqrt(2)) %>%
  mutate(sig_level = c("a", "ab", "b"),
         season_seeded = recode(season_seeded, "fall-winter" = "fall"))


plot_pr <- 
  ggplot(gg_wp_rich, aes(season_seeded, avg_p_rich))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_p_rich - se_p_rich, 
                    ymax = avg_p_rich + se_p_rich),
                    width = 0.05, size = 1.5)+
  geom_text(aes(season_seeded, y = 10 + avg_p_rich, label = sig_level), size = 5)+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = "Prairie Species Richness")

gg_pr_pi <-
  sub_cov_p %>%
  select(pf_pi_logit, pg_pi_logit, prairie_pi_logit, season_seeded) %>%
  group_by(season_seeded) %>%
  summarize(avg_pf_pi = mean(pf_pi_logit),
            sd_pf_pi  = sd(pf_pi_logit),
            se_pf_pi  = sd_pf_pi/sqrt(2),
            avg_pg_pi = mean(pg_pi_logit),
            sd_pg_pi  = sd(pg_pi_logit),
            se_pg_pi  = sd_pg_pi/sqrt(2),
            avg_pra_pi = mean(prairie_pi_logit),
            sd_pra_pi  = sd(prairie_pi_logit),
            se_pra_pi  = sd_pra_pi/sqrt(2)) %>%
  mutate(sig_level_pf  = c("a", "b", "c"),
         sig_level_pra = c("a", "a", "b"),
         season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         season_seeded = stringr::str_to_title(season_seeded))

plot_pf <- 
  ggplot(gg_pr_pi, aes(season_seeded, avg_pf_pi))+
  geom_point(aes(color = season_seeded), stat = "identity", fill = "white", size = 6)+
  geom_errorbar(aes(ymin = avg_pf_pi - se_pf_pi,
                    ymax = avg_pf_pi + se_pf_pi),
                width = 0.06, size = 1)+
  geom_text(aes(season_seeded, y = 1, label = sig_level_pf), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(-2.5, 2))+
  ggtitle("B. Forbs")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))
plot_pf
plot_pg <- 
  ggplot(gg_pr_pi, aes(season_seeded, avg_pg_pi))+
  geom_point(aes(color = season_seeded), stat = "identity", fill = "white", size = 6)+
  geom_errorbar(aes(ymin = avg_pg_pi - se_pg_pi,
                    ymax = avg_pg_pi + se_pg_pi),
                width = 0.06, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(-2.5, 2))+
  ggtitle("C. Grasses")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))
plot_pra <- 
  ggplot(gg_pr_pi, aes(season_seeded, avg_pra_pi))+
  geom_point(aes(color = season_seeded), stat = "identity", fill = "white", size = 6)+
  geom_errorbar(aes(ymin = avg_pra_pi - se_pra_pi,
                    ymax = avg_pra_pi + se_pra_pi),
                width = 0.06, size = 1)+
  geom_text(aes(season_seeded, y = .6 + abs(avg_pra_pi), label = sig_level_pra), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(-2.5, 2))+
  ggtitle("A. All Prairie")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = "logit (Relative cover)") +
  theme(axis.title.y = element_text(size = 18),
        axis.text    = element_text(size = 14),
        plot.title  = element_text(size = 18))

library(patchwork)
plot_pra + plot_pf + plot_pg
