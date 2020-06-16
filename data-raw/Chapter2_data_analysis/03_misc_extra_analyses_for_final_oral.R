# Extra things to look into before final oral exam plus some extra figures

library(dplyr)
library(ggplot2)
library(extrafont)
library(lmerTest)
library(patchwork)
theme_set(theme_bw())
data("prairie_pi")
data("weedy_pi")
library(STRIPS2veg)
data("all_site_info")

# i. Evenness --------------------------------------------------------------

data("site_div_rich")

expl_even <- site_div_rich %>%
  mutate(shan = log(gamma_div),
         evenness = shan/log(rich)) 

e1 <- lmer(evenness ~ year + species_seeded + age_yrs + season_seeded + 
           hectares_in_strips + avg_p_a +
             (1|siteID), expl_even)
summary(e1)
anova(e1)

# nix avg_p_a
e2 <- lmer(evenness ~ year + species_seeded + age_yrs + season_seeded + 
             hectares_in_strips + 
             (1|siteID), expl_even)
summary(e2)
anova(e1, e2)
anova(e2)

# nix age
e3 <- lmer(evenness ~ year + species_seeded + season_seeded + 
             hectares_in_strips + 
             (1|siteID), expl_even)
summary(e3)
anova(e2, e3)
anova(e3)

# nix season 
e4 <- lmer(evenness ~ year + species_seeded + 
             hectares_in_strips + 
             (1|siteID), expl_even)
summary(e4)
anova(e3, e4)
anova(e4)
ggResidpanel::resid_panel(e4)
performance::r2(e4)
performance::check_model(e4)

# final model is e4. Seedmix richness is still positively associated with 
# species evenness. Site size technically not associated with evenness, although
# just barely. 

# ii. Plots of variation of diversity and relative cover in 2019 -----------

data("site_div_rich")
data("veg_site")

veg_graph <- 
  veg_site %>%
  filter(year == 2019) %>%
  filter(!(is.na(group))) %>%
  mutate(group_simple = dplyr::recode(group_simple, "prairie grass" = "prairie",
                                                    "prairie forb"  = "prairie",
                                                    "weedy grass"   = "weedy",
                                                    "weedy forb"    = "weedy",
                                                    "woody"         = "tree")) %>%
  group_by(siteID, group_simple) %>%
  summarize(cov_sum = sum(cov)) %>%
  mutate(tot_cov = sum(cov_sum),
         rel_cov = cov_sum/tot_cov) %>%
  ungroup() %>%
  mutate(group_simple   = forcats::as_factor(group_simple),
         siteID         = forcats::as_factor(siteID))

ord <- veg_graph %>% filter(group_simple == "prairie") %>% arrange(desc(rel_cov)) %>% 
  magrittr::extract2("siteID")

p1 <- veg_graph %>%
  mutate(siteID = factor(siteID, ord)) %>%
  ggplot(aes(siteID, rel_cov))+
  geom_col(aes(fill = factor(group_simple, 
                             levels = c("prairie", "weedy", "tree", "other"))),
           position = position_stack(reverse = TRUE))+
  scale_fill_manual(values = c( "#4C7A3D", "#D3BE17", "#4B2A17", "#F4AB00"))+
  geom_hline(yintercept = 0.50, lty = 2)+
  labs(y = "Relative cover",
       x = NULL,
       fill = "Vegetation type")+
  coord_flip()+
  theme_bw()+
  theme(legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, family = "Fira Sans"),
        axis.title  = element_text(size = 14, family = "Fira Sans"),
        legend.text = element_text(size = 12, family = "Fira Sans"),
        legend.title = element_text(size = 14, family = "Fira Sans"),
        legend.background = element_rect(color = "black"))

p2 <- site_div_rich %>%
  filter(year == 2019) %>%
  mutate(siteID = factor(siteID, ord)) %>%
  ggplot(aes(siteID, gamma_div))+
  geom_col(fill = "#4C7A3D")+
  labs(x = NULL,
       y = "Gamma Diversity (e^H')")+
  coord_flip() +
  theme_bw()+
  theme(axis.text.x  = element_text(size = 12, family = "Fira Sans"),
        axis.title.x = element_text(size = 14, family = "Fira Sans"),
        axis.text.y = element_blank())

library(patchwork) 
p1 + p2


# iii. Plotting the R2 values for each model to compare variation explained -------------------

# making a dataframe with this information (taken from thesis, but can also be found across
# scripts)

varex <- data.frame(var = c("Gamma", "Beta", "Alpha","Prairie - gamma", "Weedy - gamma",
                            "Prairie - alpha", "Weedy - alpha", "All prairie", "C4 grass", "C3 grass",
                            "Non-leg Forb", "Legumes", "Weeds", "Weeds - Perennial", "Weeds - Annual"),
                    marR2 = c(0.56, 0.31, 0.20, 0.50, 0.16, 0.07, 0.03, 0.02, 0.13, 0.13, 0.10,
                              0.46, 0.01, 0.08, 0.32),
                    condR2 = c(0.88, 0.85, 0.45, 0.81, 0.65, 0.65, 0.57, 0.93, 0.95, 0.90, 0.92,
                               0.71, 0.93, 0.93, 0.53),
                    numFixEff = c(3, 2, 4, 3, 2, 3, 2, 2, 2, 2, 2, 4, 2, 2, 3),
                    category = c("Diversity", "Diversity", "Diversity", "Richness", "Richness", "Richness",
                                 "Richness", "Cover", "Cover", "Cover", "Cover","Cover","Cover","Cover","Cover"))
varex2 <- varex %>%
  mutate(randEffR2 = condR2 - marR2,
         errorR2 = 1 - condR2) %>%
  tidyr::pivot_longer(cols = c(marR2, condR2, randEffR2, errorR2), names_to = "R2_type",
                      values_to = "R2")
varex2

div_r2 <- varex2 %>%
  filter(R2_type == "marR2" | R2_type == "randEffR2") %>%
  filter(category == "Diversity") %>%
  ggplot(aes(var, R2)) +
  geom_col(aes(fill = factor(R2_type, levels = c("randEffR2", "marR2"))))+
  geom_text(aes(var, 0.1, label = numFixEff), family = "Fira Sans", size = 5)+
  guides(fill = FALSE) +
  labs(x = NULL, y = "Variance Explained")+
  ggtitle("Diversity")+
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values = c("#205713", "#8AA467"))+
  theme(axis.text.x = element_text(size = 14, family = "Fira Sans"),
        axis.text.y = element_text(size = 12, family = "Fira Sans"),
        axis.title.y = element_text(size = 14, family = "Fira Sans"),
        title = element_text(size = 15, family = "Fira Sans"),
        plot.title.position = "plot")
div_r2    

rich_r2 <- varex2 %>%
  filter(R2_type == "marR2" | R2_type == "randEffR2") %>%
  filter(category == "Richness") %>%
  ggplot(aes(var, R2)) +
  geom_col(aes(fill = factor(R2_type, levels = c("randEffR2", "marR2"))))+
  geom_text(aes(var, 0.05, label = numFixEff), family = "Fira Sans", size = 5)+
  guides(fill = FALSE) +
  labs(x = NULL, y = NULL)+
  ggtitle("Richness")+
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values = c("#205713", "#8AA467"))+
  theme(axis.text.x = element_text(size = 14, family = "Fira Sans"),
        axis.text.y = element_text(size = 12, family = "Fira Sans"),
        axis.title.y = element_text(size = 14, family = "Fira Sans"),
        title = element_text(size = 15, family = "Fira Sans"),
        plot.title.position = "plot")
rich_r2

cover_r2 <- 
  varex2 %>%
  filter(R2_type == "marR2" | R2_type == "randEffR2") %>%
  filter(category == "Cover") %>%
  ggplot(aes(var, R2)) +
  geom_col(aes(fill = factor(R2_type, levels = c("randEffR2", "marR2"))))+
  geom_text(aes(var, 0.05, label = numFixEff), family = "Fira Sans", size = 5)+
  guides(fill = FALSE) +
  labs(x = NULL, y = "Variance Explained")+
  ggtitle("Relative Cover")+
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values = c("#205713", "#8AA467"))+
  theme(axis.text.x = element_text(size = 14, family = "Fira Sans"),
        axis.text.y = element_text(size = 12, family = "Fira Sans"),
        axis.title.y = element_text(size = 14, family = "Fira Sans"),
        title = element_text(size = 15, family = "Fira Sans"),
        plot.title.position = "plot")

(div_r2 + rich_r2)/cover_r2

# iv. Exploring why sampling year matters... ----------------------------

# sampling year significant for: all prairie species, C4 grasses, perennial weeds
# why is prairie cover lower in 2019?

expl_sub <- prairie_pi %>%
  filter(!(siteID %in% c("ARM", "MCN", "RHO", "WHI", "WOR")))

expl_sub_w <- weedy_pi %>%
  filter(!(siteID %in% c("ARM", "MCN", "RHO", "WHI", "WOR")))

# plot all and explore

#1. All prairie spp - cover goes down in 2019
mp <- lmer(prairie_pi_logit ~ year + species_seeded + (1|siteID), expl_sub)
summary(mp)
p1 <- expl_sub %>%
  filter(!(is.na(species_seeded))) %>%
  ggplot(aes(siteID, prairie_pi, fill  = year))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Prairie Cover")
p1

# 1. C4 grasses - cover goes down in 2019 
g1 <- lmer(c4_pi_logit ~ year + species_seeded + (1|siteID), expl_sub)
summary(g1)

p2 <- expl_sub %>%
  filter(!(is.na(species_seeded))) %>%
  ggplot(aes(siteID, c4_pi, fill  = year))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("C4 grass cover")

# which grass cover goes down?? 
veg_site %>%
  filter(stringr::str_detect(group, "^prairie C4") & life_cycle == "perennial") %>%
  group_by(speciesID) %>%
  summarize(tot_cov = sum(cov)) %>%
  arrange(desc(tot_cov))

tcs <- veg_site %>%
  select(year, siteID, total_cover) %>%
  distinct()

grass_expl <- veg_mid %>%
  select(year, siteID, andge, sornu, panvi, schsc, boucu) %>%
  group_by(siteID, year) %>%
  summarize_all(~sum(.)) %>%
  left_join(tcs, by = c("siteID", "year")) %>%
  mutate_at(vars(andge:boucu), ~./total_cover) %>%
  mutate_at(vars(andge:boucu), ~ if_else(. == 0,0.0001 , .)) %>%
  left_join(all_site_info)

# andge - doesn't appear to be this...
a1 <- lmer(car::logit(andge) ~ year + species_seeded + (1|siteID), grass_expl)
summary(a1)
ggResidpanel::resid_panel(a1)
performance::r2(a1)

# sornu - goes down in 2019 
s1 <- lmer(car::logit(sornu) ~ year + species_seeded + (1|siteID), grass_expl)
summary(s1)
ggResidpanel::resid_panel(s1)
performance::r2(s1)

# panvi - nope
pv1 <- lmer(car::logit(panvi) ~ year + species_seeded + (1|siteID), grass_expl)
summary(pv1)
ggResidpanel::resid_panel(pv1)
performance::r2(pv1)

# schsc - nope
sc1 <- lmer(car::logit(schsc) ~ year + species_seeded + (1|siteID), grass_expl)
summary(sc1)
ggResidpanel::resid_panel(sc1)
performance::r2(sc1)

# boucu - goes down 
bc1 <- lmer(car::logit(boucu) ~ year + species_seeded + (1|siteID), grass_expl)
summary(bc1)
ggResidpanel::resid_panel(bc1)
performance::r2(bc1)

grass_expl %>%
  ggplot(aes(year, car::logit(boucu)))+
  geom_boxplot()+
  geom_point()

# 3. perennial weed cover goes up in 2019 - could this be due to certain spp becoming more abundant?
p3 <- expl_sub_w %>%
  filter(!(is.na(species_seeded))) %>%
  ggplot(aes(siteID, wp_pi_logit, fill  = year))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Perennial Weed cover")
p3 # largely due SLO, POW, MUG, ISB, GES
e1 <- lmer(wp_pi_logit ~ year+species_seeded +
             (1|siteID), expl_sub_w)
summary(e1)
anova(e1)      # similar p-value when we subset dataframe

# explore which species that is...

# highest abundant perennial weedy spp OVERAll
data("veg_site")
data("veg_mid")
veg_site %>%
  filter(stringr::str_detect(group, "^weedy") & life_cycle == "perennial") %>%
  group_by(speciesID) %>%
  summarize(tot_cov = sum(cov)) %>%
  arrange(desc(tot_cov))

tcs <- veg_site %>%
  select(year, siteID, total_cover) %>%
  distinct()

weeds_expl <- veg_mid %>%
  select(year, siteID, broin, cirar, poapr, tarof, phaar) %>%
  group_by(siteID, year) %>%
  summarize_all(~sum(.)) %>%
  left_join(tcs, by = c("siteID", "year")) %>%
  mutate_at(vars(broin:phaar), ~./total_cover) %>%
  mutate_at(vars(broin:phaar), ~ if_else(. == 0,0.0001 , .)) %>%
  left_join(all_site_info) 

# broin - significantly increased with year
b1 <- lmer(broin ~ year + species_seeded + (1|siteID), weeds_expl)
summary(b1)
b2 <- lmer(car::logit(broin) ~ year + species_seeded + (1|siteID), weeds_expl)
summary(b2)
performance::r2(b2)
ggResidpanel::resid_compare(list(b1, b2)) # b1 looks better although there is no transformation
performance::compare_performance(b1, b2)

# cirar - less evidence that this is driving pattern
c1 <- lmer(cirar ~ year + species_seeded + (1|siteID), weeds_expl)
summary(c1)
c2 <- lmer(car::logit(cirar) ~ year + species_seeded + (1|siteID), weeds_expl)
summary(c2)
performance::r2(c2)
ggResidpanel::resid_panel(c1)

# poapr - no...
p1 <- lmer(poapr ~ year + species_seeded + (1|siteID), weeds_expl)
summary(p1)
p2 <- lmer(car::logit(poapr) ~ year + species_seeded + (1|siteID), weeds_expl)
summary(p2)
performance::r2(p2)
ggResidpanel::resid_panel(p2)

# tarof - no....
t1 <- lmer(tarof ~ year + species_seeded + (1|siteID), weeds_expl)
summary(t1)
t2 <- lmer(car::logit(tarof) ~ year + species_seeded + (1|siteID), weeds_expl)
summary(t2)
performance::r2(t2)
ggResidpanel::resid_panel(t2)

# phaar - 
ph1 <- lmer(phaar ~ year + species_seeded + (1|siteID), weeds_expl)
summary(ph1)
ph2 <- lmer(car::logit(phaar) ~ year + species_seeded + (1|siteID), weeds_expl)
summary(ph2)
performance::r2(ph2)
ggResidpanel::resid_panel(ph2)

# so perhaps brome is increasing, but also only signifcant when data is untransformed...meh


# v. Exploring why changin PHAAR from native to exotic matters... -----------
data("veg_site")

veg_site %>%
  filter(siteID != "WAT") %>%
  filter(speciesID == "phaar") %>%
  arrange(siteID, year)

# vi. Making figure for supplementary material, all seasons relative cover of prairie spp -----

data("prairie_pi")

season_plot <- prairie_pi %>%
  filter(year == "2019") %>%
  group_by(season_seeded) %>%
  summarize(n              = n(),
            avg_prairie_pi = mean(prairie_pi),
            se_prairie_pi  = sd(prairie_pi)/sqrt(n),
            avg_pg_pi      = mean(pg_pi),
            se_pg_pi       = sd(pg_pi)/sqrt(n),
            avg_pf_pi      = mean(pf_pi),
            se_pf_pi       = sd(pf_pi)/sqrt(n)) %>%
  mutate(season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         season_seeded = stringr::str_to_title(season_seeded))

p1 <- season_plot %>%
  ggplot(aes(season_seeded, avg_prairie_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_prairie_pi - se_prairie_pi, 
                    ymax = avg_prairie_pi + se_prairie_pi),
                width = 0.05, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 0.8))+
  guides(color = FALSE)+
  ggtitle("A. Prairie")+
  labs(x = NULL, 
       y = "Relative Cover")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
p1
p2 <- season_plot %>%
  ggplot(aes(season_seeded, avg_pf_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_pf_pi - se_pf_pi, 
                    ymax = avg_pf_pi + se_pf_pi),
                width = 0.05, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 0.8))+
  guides(color = FALSE)+
  ggtitle("B. Forbs")+
  labs(x = NULL, 
       y = NULL)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
p2
p3 <- season_plot %>%
  ggplot(aes(season_seeded, avg_pg_pi))+
  geom_bar(aes(color = season_seeded), stat = "identity", fill = "white", size = 2)+
  geom_errorbar(aes(ymin = avg_pg_pi - se_pg_pi, 
                    ymax = avg_pg_pi + se_pg_pi),
                width = 0.05, size = 1)+
  scale_color_grey(start = 0.2, end = 0.7)+
  scale_y_continuous(limits = c(0, 0.8))+
  guides(color = FALSE)+
  ggtitle("C. Grasses")+
  labs(x = NULL, 
       y = NULL)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
p3

p1 + p2 + p3
