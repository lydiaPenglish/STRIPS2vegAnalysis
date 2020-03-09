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

# subsets of dataframes..
sub_div_g <- site_div_rich %>%
  filter(siteID %in% sites & year == "2019")
sub_div_q <- quad_div_rich %>%
  filter(siteID %in% sites & year == "2019")
sub_cov_p <- prairie_pi %>%
  filter(siteID %in% sites & year == "2019")
sub_cov_w <- weedy_pi %>%
  filter(siteID %in% sites & year == "2019")

# ---- Diversity - wouldn't expect it to differ given same seed mix ----

# 1. gamma diversity

# size - NS
g1 <- lm(gamma_div ~ acres_in_strips, sub_div_g)
anova(g1)

# age - NS
g2 <- lm(gamma_div ~ age_yrs, sub_div_g)
anova(g2)

# season - NS
g3 <- lm(gamma_div ~ season_seeded, sub_div_g)
anova(g3)

# 2. beta diversity 

# size - NS
b1 <- lm(beta_div ~ acres_in_strips, sub_div_g)
anova(b1)

# age - NS
b2 <- lm(beta_div ~ age_yrs, sub_div_g)
anova(b2)

# season - NS
b3 <- lm(beta_div ~ season_seeded, sub_div_g)
anova(b3)

# 3. alpha diversity 

a1 <- lmer(alpha_div ~ acres_in_strips + (1|siteID), sub_div_q)
summary(a1)
anova(a1)

# age - NS
a2 <- lmer(alpha_div ~ age_yrs + (1|siteID), sub_div_q)
anova(a2)

# season - NS
a3 <- lmer(alpha_div ~ season_seeded + (1|siteID), sub_div_q)
anova(a3)

# ---- Richness of prairie and weedy spp, also don't really think it will differ ----

# 1. Gamma prairie richness

# 2. Gamma weedy richness

# 3. Alpha prairie richness

# 4. Alpha weedy richness
 
# ---- Cover of different functional groups ----

# 1. All prairie

# 2. Prairie grass

# 3. Prairie forb

# 4. All weeds

# 5. Annual weeds

# 6. Perennial weeds