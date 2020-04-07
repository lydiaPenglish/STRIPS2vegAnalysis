# Script that visualizes which species that are always, sometimes, and never found

library(tidyverse)
library(STRIPS2veg)
data("species_list")
data("veg_site")
data("extra_spp")
nat_codes <- species_list %>%                 # 5 letter codes for prairie species
  filter(str_detect(group, "^prairie")) %>%
  dplyr::select(speciesID) %>%
  unlist()
sm <- read_csv("data-raw/seed_mix_info/all_site_seed_list.csv") # seed mix for each site
my_cols <- c("#361554", "#7A4FB6", "#C7B1F2", "#E99725", 
             "#8B1C38", "#D3BE17", 
             "#8FA844", "#4C7A3D")
scales::show_col(my_cols)

# prairie species found at each site
pr_found <- 
  veg_site %>%
  filter(speciesID %in% nat_codes) %>%
  select(year, siteID, speciesID) %>%
  full_join(extra_spp)%>%                # adding in extra species
  filter(siteID != "GOS", siteID != "SME", siteID != "STT", siteID != "WAT") %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  arrange(siteID)

# comparing pr spp found with species seeded
not_founds <- anti_join(sm, pr_found) # species seeded but not found
extras <- anti_join(pr_found, sm)     # species *not* seeded but found

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
  mutate(group_simple = recode(group, "prairie sedge" = "sedge", "prairie forb" = "forb", "prairie C3 grass" = "C3 grass", "prairie C4 grass" = "C4 grass"))

# figures to visualize

# separate species that are always found or never found
always <- found_missing %>%
  filter(proportion_found == 1) %>%
  mutate(group_simple2 = case_when(family != "fabaceae" ~ group_simple,
                                   TRUE ~ "legume"),
         group_simple2 = str_to_title(recode(group_simple2, 
                                forb    = "non-leguminous forb")))
never <- found_missing %>%
  filter(proportion_found == 0) %>%
  mutate(group_simple2 = case_when(family != "fabaceae" ~ group_simple,
                                   TRUE ~ "legume"),
         group_simple2 = str_to_title(recode(group_simple2, 
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
         fam_simp = str_to_title(fam_simp)) 
# other version where I had more than one family
# sometimes_forb <-  found_missing %>%
#   filter(proportion_found > 0 & proportion_found < 1) %>%
#   filter(group_simple == "forb") %>%
#   mutate(fam_simp = case_when(family != "asteraceae" & family != "fabaceae" & family != "asclepiadaceae" ~ "other",
#                               TRUE ~ family),
#          fam_simp = stringr::str_to_title(fam_simp))

p1 <- sometimes_forb %>%
  ggplot(aes(proportion_found*100, reorder(full_name, proportion_found)))+
  geom_point(aes(size = timesSeeded, fill = str_wrap(fam_simp,15)), 
             color = "black", pch = 21, stroke = 1.23)+
  geom_segment(aes(x = 0, xend = proportion_found*100, 
                   y = full_name, yend = full_name), linetype = "longdash")+
  geom_point(aes(size = timesSeeded, fill = str_wrap(fam_simp,15)), 
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
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1), face = "italic"),
        axis.title  = element_text(size = rel(1.15)),
        plot.title  = element_text(size = rel(1.3), face= "bold"),
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
        axis.text.y  = element_text(size = rel(1.1), face = "italic"),
        axis.text.x  = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.15)),
        plot.title = element_text(size = rel(1.3), face= "bold"),
        plot.title.position = "plot")
p2
library(patchwork)
p1 + p2

always %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, 
             fill = str_wrap(group_simple2,15)))+
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
        axis.text.x  = element_text(size = rel(1.1)),
        axis.text.y  = element_text(size = rel(1.1), face = "italic"),
        axis.title = element_text(size = rel(1.15)),
        plot.title = element_text(size = rel(1.3), hjust = 0.5, face= "bold"))

never %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, 
             color = group_simple2, fill = str_wrap(group_simple2,15)))+
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
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1), face = "italic"),
        axis.title  = element_text(size = rel(1.15)),
        plot.title  = element_text(size = rel(1.3), hjust = 0.5, face= "bold"))

# as an aside, how many praire species did we find ....

pr_found %>%
  select(speciesID) %>%
  distinct() %>%
  tally()
