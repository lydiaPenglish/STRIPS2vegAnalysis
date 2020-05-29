# Extra things to look into before final oral exam plus some extra figures

library(dplyr)
library(ggplot2)
library(extrafont)
library(lmerTest)
library(patchwork)
theme_set(theme_bw())

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
