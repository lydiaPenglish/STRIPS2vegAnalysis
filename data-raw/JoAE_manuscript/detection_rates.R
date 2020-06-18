# Script look at detection rate of seed mix across sites and species specific detection rates


# Libraries and data loading/organization ----------------------------------------
library(STRIPS2veg)
library(dplyr)
library(ggplot2)
data("species_list")
data("all_site_info")
data("veg_site")
data("extra_spp")
data("strips")

pa_rat <- strips %>%
  mutate(perim_area = perimeter/area) %>%
  group_by(siteID) %>%
  summarize(n_strips = n(),
            avg_p_a  = mean(perim_area))

all_site_info <- left_join(all_site_info, pa_rat)

nat_codes <- species_list %>% # 5 letter codes for prairie species
  filter(stringr::str_detect(group, "^prairie")) %>%
  dplyr::select(speciesID) %>%
  unlist()

# seed mix of each site 
sm <- readr::read_csv("data-raw/seed_mix_info/all_site_seed_list.csv") # seed mix for each site

# adding in extras species found at each site
pr_found <- 
  veg_site %>%
  filter(speciesID %in% nat_codes) %>%
  select(year, siteID, speciesID) %>%
  full_join(extra_spp)%>%               
  # filtering out sites that we don't have seed mix info for
  filter(siteID != "GOS", siteID != "SME", siteID != "STT", siteID != "WAT") %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  arrange(siteID)

extras <- anti_join(pr_found, sm)     # species *not* seeded but still found

sm_found <- pr_found %>%
  anti_join(., extras) %>%            # get rid of species not seeded at each site
  group_by(siteID) %>%
  summarize(n_found = n()) %>%
  left_join(., sm %>% group_by(siteID) %>% summarize(n_seeded = n())) %>%
  mutate(prop_found = n_found/n_seeded) %>%     # prop_found = detection rate
  left_join(., all_site_info, by = "siteID")

# Seed mix detection across sites --------------------------------------------

# graph of the detection rate across sites
p1 <- sm_found %>%
  mutate(siteID = forcats::fct_reorder(siteID, desc(prop_found))) %>%
  ggplot(aes(siteID, prop_found))+
  geom_point(aes(size = n_seeded))+
  labs(x = NULL, y = "Proportion of seed mix detected",
       size = "Number of \nspecies seeded")+
  coord_flip()+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        axis.text.y = element_text(face = "bold", size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.2)))+
  scale_y_continuous(limits = c(0.4, 1))
p1

sm_found %>%
  ggplot(aes(species_seeded, prop_found))+
  geom_point(size = 2, color = "#7A4FB6")+
  geom_hline(yintercept = 0.5, lty = 2)+
  scale_y_continuous(limits = c(0.25, 1), breaks = c(.25, 0.50, 0.75, 1))+
  labs(x = "Seedmix Richness",
       y = "Proportion of Seedmix \nDetected")+
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
# Doesn't look like seeding more or less species increases detection rate

# Models # # # # 

l_full <- lm(car::logit(prop_found) ~ n_seeded + age_yrs + 
               log(hectares_in_strips) + log(avg_p_a) + 
               season_seeded,
             data = sm_found)
anova(l_full)

# nix p_a ratio

l1 <- lm(car::logit(prop_found) ~ n_seeded + age_yrs + 
           log(hectares_in_strips) + 
           season_seeded,
         data = sm_found)
anova(l1, l_full)      # out! 
anova(l1)

# get rid of age

l2 <- lm(car::logit(prop_found) ~ n_seeded + log(hectares_in_strips) + 
           season_seeded,
         data = sm_found)
anova(l1, l2)          # out! 
anova(l2)

# get rid of season
l3 <- lm(car::logit(prop_found) ~ n_seeded  + log(hectares_in_strips),
         data = sm_found)
anova(l2, l3)          # out!
anova(l3)

# get rid of size
l4 <- lm(car::logit(prop_found) ~ n_seeded,
         data = sm_found)
anova(l4, l3)

# also try just size 
l4 <- lm(car::logit(prop_found) ~ log(hectares_in_strips),
         data = sm_found)
anova(l4)

# Extra: modeling detection rate as a function of weeds (not sig.) -------------------------

weeds <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy")) %>%
  select(speciesID) %>%
  unlist()

# weedy cover
weed_cov <-
  veg_site %>%
  filter(speciesID %in% weeds) %>%
  group_by(year, siteID, total_cover) %>%
  summarize(w_cov = sum(cov)) %>%
  mutate(w_pi = w_cov/total_cover) %>%
  group_by(siteID) %>%
  summarize(mean_cov  = mean(w_cov),
            mean_w_pi = mean(w_pi)) %>%
  left_join(sm_found)

#model
w1 <- lm(car::logit(prop_found) ~ mean_w_pi, data = weed_cov)
summary(w1)
ggResidpanel::resid_panel(w1)

wp1 <- weed_cov %>%
  ggplot(aes(mean_w_pi, car::logit(prop_found)))+
  geom_smooth(method = "lm", lty = 2, size = 2, color = "black")+
  geom_point(color = "grey20", size = 2)+
  labs(x = "Relative cover of \nweedy species",
       y = "logit(Detection rate)")+
  theme_bw()+
  theme(axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))
wp1

#  Richness
weed_rich <- veg_site %>%
  filter(speciesID %in% weeds) %>%
  ungroup() %>%
  select(siteID, speciesID) %>%
  distinct() %>%
  group_by(siteID) %>%
  summarize(w_rich = n()) %>%
  left_join(sm_found, by = "siteID")

w2 <- lm(car::logit(prop_found) ~ w_rich, weed_rich)
summary(w2)
anova(w2)
ggResidpanel::resid_panel(w2)

wp2 <- weed_rich %>%
  ggplot(aes(w_rich, car::logit(prop_found)))+
  geom_smooth(method = "lm", lty = 2, size = 2, color = "black")+
  geom_point(color = "grey20", size = 2)+
  labs(x = "Weed species richness",
       y = NULL)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.title  = element_text(size = 14),
        axis.text.x = element_text(size = 12))
wp2



# Looking at species specific detection rates and making supplementary figs ------
my_cols <- c("#361554", "#7A4FB6", "#C7B1F2", "#E99725", 
             "#8B1C38", "#D3BE17", 
             "#8FA844", "#4C7A3D")

not_founds <- anti_join(sm, pr_found)
# number of times something was seeded
times_seeded <- sm %>%
  group_by(speciesID)%>%
  summarize(timesSeeded = n())

# number of times something wasn't found
times_missing <- not_founds %>%
  group_by(speciesID)%>%
  summarize(timesMissing = n())

# proportion of times things were found
found_missing <- left_join(times_seeded, times_missing, by = "speciesID") %>%
  replace(is.na(.), 0) %>%
  mutate(proportion_missing = timesMissing/timesSeeded,
         proportion_found  = 1 - proportion_missing) %>%
  left_join(., species_list, by = "speciesID") %>%
  mutate(group_simple = recode(group, 
                               "prairie sedge" = "sedge", 
                               "prairie forb" = "forb", 
                               "prairie C3 grass" = "C3 grass", 
                               "prairie C4 grass" = "C4 grass"))

# separate species that are always found or never found
always <- found_missing %>%
  filter(proportion_found == 1) %>%
  mutate(group_simple2 = case_when(family != "fabaceae" ~ group_simple,
                                   TRUE ~ "legume"),
         group_simple2 = stringr::str_to_title(recode(group_simple2, 
                                                      forb    = "non-leguminous forb")))
never <- found_missing %>%
  filter(proportion_found == 0) %>%
  mutate(group_simple2 = case_when(family != "fabaceae" ~ group_simple,
                                   TRUE ~ "legume"),
         group_simple2 = stringr::str_to_title(recode(group_simple2, 
                                                      forb    = "non-leguminous forb")))
# further separating grasses from forbs
sometimes_grass <- found_missing %>%
  filter(proportion_found > 0 & proportion_found < 1) %>%
  filter(group_simple == "C3 grass" | group_simple == "C4 grass" | group_simple == "sedge")
sometimes_forb <- found_missing %>%
  filter(proportion_found > 0 & proportion_found < 1) %>%
  filter(group_simple == "forb") %>%
  mutate(fam_simp = case_when(family != "fabaceae" ~ "other",
                              TRUE ~ family),
         fam_simp = recode(fam_simp, 
                           fabaceae = "legume",
                           other    = "non-leguminous forb"),
         fam_simp = stringr::str_to_title(fam_simp)) 

# # FIGURES # # 
p1 <- sometimes_forb %>%
  ggplot(aes(proportion_found*100, reorder(full_name, proportion_found)))+
  geom_point(aes(size = timesSeeded, fill = stringr::str_wrap(fam_simp,15)), 
             color = "black", pch = 21, stroke = 1.23)+
  geom_segment(aes(x = 0, xend = proportion_found*100, 
                   y = full_name, yend = full_name), linetype = "longdash")+
  geom_point(aes(size = timesSeeded, fill = stringr::str_wrap(fam_simp,15)), 
             color = "black", pch = 21, stroke = 1.23)+
  geom_vline(xintercept = 50, linetype = "dotted", size = 1)+
  scale_fill_manual(values = c(my_cols[1], my_cols[3]))+
  labs(y = NULL,
       x = "Percent detection (%)",
       size = "Times Seeded",
       fill = "Functional Group",
       title = "A. Forbs")+
  guides(fill = guide_legend(override.aes = list(size=5),
                             order = 1),
         size = guide_legend(order = 0))+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.text = element_text(family = "Fira Sans"),
        legend.title = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_text(size = rel(1.1), family = "Fira Sans"),
        axis.text.y = element_text(size = rel(1.1), face = "italic", family = "Fira Sans"),
        axis.title  = element_text(size = rel(1.15), family = "Fira Sans"),
        plot.title  = element_text(size = rel(1.3), face= "bold", family = "Fira Sans"),
        plot.title.position = "plot")
p1
p2 <- sometimes_grass %>%
  ggplot(aes(proportion_found*100, reorder(full_name, proportion_found)))+
  geom_point(aes(fill = group_simple, size = timesSeeded), 
             color = "black", pch = 21, stroke = 1.23)+
  geom_segment(aes(x = 0, xend = proportion_found*100, 
                   y = full_name, yend = full_name), linetype = "longdash")+
  geom_point(aes(fill = group_simple, size = timesSeeded), 
             color = "black", pch = 21, stroke = 1.23)+
  geom_vline(xintercept = 50, linetype = "dotted", size = 1)+
  scale_fill_manual(values = my_cols[6:8])+
  labs(y = NULL,
       x = "Percent detection (%)",
       size = "Times Seeded",
       fill = "Functional Group",
       title = "B. Grasses")+
  guides(fill = guide_legend(override.aes = list(size=5),
                             order = 1),
         size = guide_legend(order = 0))+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.text = element_text(family = "Fira Sans"),
        legend.title = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_text(size = rel(1.1), family = "Fira Sans"),
        axis.text.y = element_text(size = rel(1.1), face = "italic", family = "Fira Sans"),
        axis.title  = element_text(size = rel(1.15), family = "Fira Sans"),
        plot.title  = element_text(size = rel(1.3), face= "bold", family = "Fira Sans"),
        plot.title.position = "plot")
p2
library(patchwork)
p1 + p2

sometimes_p <- p1 + p2
ggsave("sometimes_detected.png", plot = sometimes_p, dpi = 600, width = 10, height = 8)


always_p <- always %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, 
             fill = stringr::str_wrap(group_simple2,15)))+
  geom_bar(aes(color = group_simple2),
           stat = "identity", size = 1)+
  geom_hline(yintercept = 5, lty = 2)+
  coord_flip()+
  scale_color_manual(values = c(my_cols[6:7], my_cols[1], my_cols[3], my_cols[8]))+
  scale_fill_manual (values = alpha(c(my_cols[6:7], my_cols[1], my_cols[3], my_cols[8]), .4))+
  labs(x = NULL,
       y= "# of times seeded",
       fill = "Functional group")+
  guides(fill = 
           guide_legend(override.aes = 
                          list(color = c(my_cols[6:7], my_cols[1], my_cols[3], my_cols[8])
                          )),
         color = FALSE)+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.text  = element_text(family = "Fira Sans"),
        legend.title = element_text(family = "Fira Sans"),
        axis.text.x  = element_text(size = rel(1.1)),
        axis.text.y  = element_text(size = rel(1.1), face = "italic", family = "Fira Sans"),
        axis.title   = element_text(size = rel(1.15), family = "Fira Sans"),
        plot.title   = element_text(size = rel(1.3), hjust = 0.5, face= "bold", 
                                    family = "Fira Sans"))
always_p
ggsave(filename = "always_detected.png", always_p, dpi = 600, width = 7, height = 5)

never_p <- never %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, 
             color = group_simple2, fill = stringr::str_wrap(group_simple2,15)))+
  geom_bar(stat = "identity", size = 1)+
  coord_flip()+
  theme_bw()+
  scale_color_manual(values = c(my_cols[6], 
                                my_cols[1], 
                                my_cols[3], 
                                my_cols[8]))+
  scale_fill_manual (values = alpha(c(my_cols[6], 
                                      my_cols[1], 
                                      my_cols[3], 
                                      my_cols[8]), .4))+
  labs(x = NULL,
       y = "# of times seeded",
       fill = "Functional group")+
  guides(fill = 
           guide_legend(override.aes = 
                          list(color = c(my_cols[6], 
                                         my_cols[1], 
                                         my_cols[3], 
                                         my_cols[8])
                          )),
         color = FALSE)+
  theme(legend.background = element_rect(color = "black"),
        axis.text.x = element_text(size = 12, family = "Fira Sans"),
        axis.text.y = element_text(size = 12, face = "italic", family = "Fira Sans"),
        axis.title  = element_text(size = 14, famil = "Fira Sans"),
        plot.title  = element_text(size = rel(1.3), hjust = 0.5, face= "bold"),
        legend.text = element_text(size = 12, family = "Fira Sans"),
        legend.title = element_text(size = 14, family = "Fira Sans"))
never_p
ggsave("never_detected.png", never_p, dpi = 600, width = 6, height = 8)