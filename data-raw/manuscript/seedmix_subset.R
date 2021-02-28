# Case study - sites that had the same seed mix planted #
# Statewide Mesic 10-30 Iowa Pollinator Mix Allendan
# ARM, WOR, MRS, NYK, SER, RHO

# Load libraries and subset dataframes --------------------------------------

library(STRIPS2veg)
library(dplyr)
library(ggplot2)
library(lmerTest)
library(emmeans)
library(patchwork)
library(extrafont)
sites <- c("ARM", "WOR", "MRS", "NYK", "SER", "RHO")
data("site_div_rich")
data("quad_div_rich")
data("prairie_pi")
data("weedy_pi")
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


# Richness of prairie and weedy spp -----------------------------------

# 1.Prairie richness
sub_div_g %>%
  ggplot(aes(siteID, p_rich))+
  geom_col()

#        size - NS
p1 <- lm(log(p_rich) ~ log(hectares_in_strips), sub_div_g)
summary(p1)

#        age - NS
p2 <- lm(log(p_rich) ~ age_yrs,  sub_div_g)
summary(p2)

#        season - sig
p3 <- lm(p_rich ~ season_seeded, sub_div_g)
summary(p3)
anova(p3)
ggResidpanel::resid_panel(p3)
p3m <- emmeans(p3, ~season_seeded,)
summary(p3m)
pairs(p3m) %>%
  confint()# difference between fall and summer planting. Spring not diff from either. 
contrast(p3m) %>%
  confint()

# 2. Weedy richness - NS for anything
sub_div_g %>%
  ggplot(aes(siteID, w_rich))+
  geom_col()

#        size - NS
w1 <- lm(log(w_rich) ~ log(acres_in_strips), sub_div_g)
summary(w1)

#        age - NS
w2 <- lm(log(w_rich) ~ age_yrs, sub_div_g)
summary(w2)

#        season - NS
w3 <- lm(w_rich ~ season_seeded, sub_div_g)
summary(w3)
anova(w3)

#       perim:area - NS
w4 <- lm(log(w_rich) ~ avg_p_a, sub_div_g)
anova(w4)

# Cover of different functional groups --------------------------------------

# 1. All prairie, season matters
sub_cov_p %>%
  ggplot(aes(siteID, prairie_pi_logit))+
  geom_col()

#         size - NS
pp1 <- lm(prairie_pi_logit ~ log(hectares_in_strips), sub_cov_p)
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
pairs(pp3_m) %>% confint()
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
pf1 <- lm(pf_pi_logit ~ log(hectares_in_strips), sub_cov_p)
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
ww1 <- lm(weed_pi_logit ~ log(hectares_in_strips), sub_cov_w)
summary(ww1)

#         age - NS
ww2 <- lm(weed_pi_logit ~ age_yrs, sub_cov_w)
summary(ww2)

#         season - sig
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
  ggplot(aes(siteID, wa_pi))+
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

#         season - almost but not sig
wp3 <- lm(wp_pi_logit ~ season_seeded, sub_cov_w)
summary(wp3)
anova(wp3)

#       perim:area - NS
wp4 <- lm(wp_pi_logit ~ avg_p_a, sub_cov_w)
anova(wp4)

# Make figures ------------------------------------------------------
# 1. Prairie and weedy species richness
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
         season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         season_seeded = stringr::str_to_title(season_seeded))

plot_pr <- 
  ggplot(gg_wp_rich, aes(season_seeded, avg_p_rich))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_p_rich - se_p_rich, 
                    ymax = avg_p_rich + se_p_rich),
                width = 0.05, size = 1.5)+
  geom_text(aes(season_seeded, y = 10 + avg_p_rich, label = sig_level), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 40))+
  guides(color = FALSE)+
  ggtitle("A. Prairie")+
  labs(x = NULL, 
       y = "Species richness")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
plot_pr

plot_wr <- 
  ggplot(gg_wp_rich, aes(season_seeded, avg_w_rich))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_w_rich - se_w_rich, 
                    ymax = avg_w_rich + se_w_rich),
                width = 0.05, size = 1.5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 40))+
  guides(color = FALSE)+
  ggtitle("B. Weedy")+
  labs(x = NULL, 
       y = NULL)+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
plot_wr

rich_plot <- plot_pr + plot_wr
ggsave("season_vs_richness.png", plot = rich_plot, dpi = 600, 
       width= 7, height = 4.5)

# 2. prairie cover

gg_pr_pi2 <-
  sub_cov_p %>%
  select(pf_pi, pg_pi, prairie_pi, season_seeded) %>%
  group_by(season_seeded) %>%
  summarize(avg_pf_pi = mean(pf_pi),
            sd_pf_pi  = sd(pf_pi),
            se_pf_pi  = sd_pf_pi/sqrt(2),
            avg_pg_pi = mean(pg_pi),
            sd_pg_pi  = sd(pg_pi),
            se_pg_pi  = sd_pg_pi/sqrt(2),
            avg_pra_pi = mean(prairie_pi),
            sd_pra_pi  = sd(prairie_pi),
            se_pra_pi  = sd_pra_pi/sqrt(2)) %>%
  mutate(sig_level_pf  = c("a", "b", "c"),
         sig_level_pra = c("a", "a", "b"),
         season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         season_seeded = stringr::str_to_title(season_seeded))

plot_pf <- 
  ggplot(gg_pr_pi2, aes(season_seeded, avg_pf_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_pf_pi - se_pf_pi,
                    ymax = avg_pf_pi + se_pf_pi),
                width = 0.06, size = 1)+
  geom_text(aes(season_seeded, y = 1, label = sig_level_pf), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1))+
  ggtitle("C. Forbs")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))
plot_pf
plot_pg <- 
  ggplot(gg_pr_pi2, aes(season_seeded, avg_pg_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_pg_pi - se_pg_pi,
                    ymax = avg_pg_pi + se_pg_pi),
                width = 0.06, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1))+
  ggtitle("B. Grasses")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))
plot_pg
plot_pra <- 
  ggplot(gg_pr_pi2, aes(season_seeded, avg_pra_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_pra_pi - se_pra_pi,
                    ymax = avg_pra_pi + se_pra_pi),
                width = 0.06, size = 1)+
  geom_text(aes(season_seeded, y = 1, label = sig_level_pra), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1))+
  ggtitle("A. All Prairie Species")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = "Relative Cover") +
  theme(axis.title.y = element_text(size = 18),
        axis.text    = element_text(size = 14),
        plot.title  = element_text(size = 18))
plot_pra
library(patchwork)
season_plot <- plot_pra + plot_pg + plot_pf 

ggsave("season_vs_relCov.png", plot = season_plot, dpi = 600, 
       width= 9, height = 4.5)

# Extra fig - weedy cover -----------------------------------------------

gg_wc <- sub_cov_w %>%
  select(weed_pi, wp_pi, wa_pi, season_seeded) %>%
  group_by(season_seeded) %>%
  summarize(avg_weed_pi = mean(weed_pi),
            sd_weed_pi  = sd(weed_pi),
            se_weed_pi  = sd_weed_pi/sqrt(2),
            avg_wp_pi = mean(wp_pi),
            sd_wp_pi  = sd(wp_pi),
            se_wp_pi  = sd_wp_pi/sqrt(2),
            avg_wa_pi = mean(wa_pi),
            sd_wa_pi  = sd(wa_pi),
            se_wa_pi  = sd_wa_pi/sqrt(2)) %>%
  mutate(sig_level_weed  = c("a", "a", "b"),
         season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         season_seeded = stringr::str_to_title(season_seeded))

plota <- ggplot(gg_wc, aes(season_seeded, avg_weed_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_weed_pi - se_weed_pi,
                    ymax = avg_weed_pi + se_weed_pi),
                width = 0.06, size = 1)+
  geom_text(aes(season_seeded, y = 1, label = sig_level_weed), size = 5)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = c(0.0, 0.25, 0.50, 0.75, 1.0))+
  ggtitle("A. All Weeds")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = "Relative Cover") +
  theme(axis.text   = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        plot.title  = element_text(size = 18))
plotb <- ggplot(gg_wc, aes(season_seeded, avg_wp_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_wp_pi - se_wp_pi,
                    ymax = avg_wp_pi + se_wp_pi),
                width = 0.06, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = c(0.0, 0.25, 0.50, 0.75, 1.0))+
  ggtitle("B. Perennials")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))

plotc <- ggplot(gg_wc, aes(season_seeded, avg_wa_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_wa_pi - se_wa_pi,
                    ymax = avg_wa_pi + se_wa_pi),
                width = 0.06, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = c(0.0, 0.25, 0.50, 0.75, 1.0))+
  ggtitle("C. Annuals")+
  guides(color = FALSE)+
  labs(x = NULL, 
       y = NULL) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        plot.title  = element_text(size = 18))

weed_plots <- plota + plotb + plotc
ggsave("season_vs_relCov_weeds.png", plot = weed_plots, dpi = 600, 
       width= 9, height = 4.5)
