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
             "#8B1C38", "#8AA467", "#4B873A", "#205713")
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
  filter(proportion_found == 1)
never <- found_missing %>%
  filter(proportion_found == 0)
# further separating grasses from forbs
sometimes_grass <- found_missing %>%
  filter(proportion_found > 0 & proportion_found < 1) %>%
  filter(group_simple == "C3 grass" | group_simple == "C4 grass" | group_simple == "sedge")
sometimes_forb <-  found_missing %>%
  filter(proportion_found > 0 & proportion_found < 1) %>%
  filter(group_simple == "forb") %>%
  mutate(fam_simp = case_when(family != "asteraceae" & family != "fabaceae" & family != "asclepiadaceae" ~ "other",
                              TRUE ~ family),
         fam_simp = stringr::str_to_title(fam_simp))

# could improve more but ok
sometimes_forb %>%
  ggplot(aes(proportion_found*100, reorder(full_name, proportion_found)))+
  geom_point(aes(size = timesSeeded, fill = fam_simp), color = "black", pch = 21, stroke = 1.23)+
  geom_segment(aes(x = 0, xend = proportion_found*100, y = full_name, yend = full_name), linetype = "longdash")+
  geom_point(aes(size = timesSeeded, fill = fam_simp), color = "black", pch = 21, stroke = 1.23)+
  geom_vline(xintercept = 50, linetype = "dotted", size = 1)+
  scale_fill_manual(values = c(my_cols[1], my_cols[3], my_cols[4:5]))+
  labs(y = NULL,
       x = "Percent detection (%)",
       size = "Times Seeded",
       fill = "Family",
       title = "Forbs that were sometimes detected")+
  guides(fill = guide_legend(override.aes = list(size=5)))+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1), face = "italic"),
        axis.title  = element_text(size = rel(1.15)),
        plot.title  = element_text(size = rel(1.3), hjust = 0.5, face= "bold"))

sometimes_grass %>%
  ggplot(aes(proportion_found*100, reorder(full_name, proportion_found)))+
  geom_point(aes(size = timesSeeded, fill = group_simple), color = "black", pch = 21, stroke = 1.23)+
  geom_segment(aes(x = 0, xend = proportion_found*100, y = full_name, yend = full_name), linetype = "longdash")+
  geom_point(aes(size = timesSeeded, fill = group_simple), color = "black", pch = 21, stroke = 1.23)+
  geom_vline(xintercept = 50, linetype = "dotted", size = 1)+
  scale_fill_manual(values = my_cols[6:8])+
  labs(y = NULL,
       x = "Percent detection (%)",
       size = "Times Seeded",
       fill = "Functional Group",
       title = "Grasses that were sometimes detected")+
  guides(fill = guide_legend(override.aes = list(size=5)))+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        axis.text.y  = element_text(size = rel(1.1), face = "italic"),
        axis.text.x  = element_text(size = rel(1.1)),
        axis.title = element_text(size = rel(1.15)),
        plot.title = element_text(size = rel(1.3), hjust = 0.5, face= "bold"))

always %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, color = group_simple, fill = group_simple))+
  geom_bar(stat = "identity", size = 1)+
  geom_hline(yintercept = 5, lty = 2)+
  coord_flip()+
  scale_color_manual(values = c("goldenrod1", "dark green", "thistle", "brown"))+
  scale_fill_manual (values = alpha(c("goldenrod1", "dark green", "thistle", "brown"), .4))+
  labs(x = NULL,
       y= "# of times seeded",
       fill = "Functional group",
       title = "Seeded species that were always detected")+
  guides(color = F)+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        axis.text.x  = element_text(size = rel(1.1)),
        axis.text.y  = element_text(size = rel(1.1), face = "italic"),
        axis.title = element_text(size = rel(1.15)),
        plot.title = element_text(size = rel(1.3), hjust = 0.5, face= "bold"))

never %>%
  ggplot(aes(reorder(full_name, timesSeeded), timesSeeded, color = group_simple, fill = group_simple))+
  geom_bar(stat = "identity", size = 1)+
  coord_flip()+
  theme_bw()+
  scale_color_manual(values = c("goldenrod1","thistle", "brown"))+
  scale_fill_manual (values = alpha(c("goldenrod1", "thistle", "brown"), .4))+
  labs(x = NULL,
       y = "# of times seeded",
       fill = "Functional group",
       title = "Seeded species that were never detected")+
  guides(color = F)+
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
