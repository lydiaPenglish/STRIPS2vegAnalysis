# Extra things to look into before final oral exam plus some extra figures

library(dplyr)
library(ggplot2)
library(extrafont)
library(lmerTest)

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
