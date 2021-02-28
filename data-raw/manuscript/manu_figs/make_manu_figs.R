### Re-make some of the figs for the manuscript

library(dplyr)
library(ggplot2)
library(STRIPS2veg)
library(gt)
library(lmerTest)
library(patchwork)
theme_set(theme_bw())

# NB: this script doesn't include ALL figures

# Figure 1 - map and table ------------------------------------------
# NB: cannot make map in public repo bc it uses centroids, but will make 
# accompanying table in `gt`

# calculating avg area:perimeter ratio of strips
data("strips")

pa_rat <- strips %>%
  mutate(perim_area = perimeter/area) %>%
  group_by(siteID) %>%
  summarize(n_strips = n(),
            avg_p_a  = mean(perim_area))

## add in ID number for map - copied these ids manually from centroid info
# in a private repo

ids <- data.frame(siteID = c("ARM", "BUE", "DMW", "GES", "GOS", 
                             "GUT", "HOE", "ISB", "MCN", "MQW",
                             "MRS", "MUG", "NYK", "POW", "RDM", 
                             "RHO", "ROD", "SER", "SLO", "SME", 
                             "SMI", "STN", "STT", "WHI", "WOR"),
                  numID  = c(21, 3, 19, 11, 10,
                             14, 1, 23, 25, 22, 
                             16, 2, 20, 24, 6 ,
                             15, 9, 18, 8 , 4 , 
                             5 , 7, 17, 12, 13))

# join everything together

site_info_tab <- 
  left_join(all_site_info, pa_rat, by = "siteID") %>%
  left_join(., ids, by = "siteID") %>%
  mutate(season_seeded = recode(season_seeded, "fall-winter" = "fall"),
         sampling_year = ifelse(siteID %in% c("ARM", "MCN", "RHO", "WOR", "WHI"), 
                                "2019", "2018 & 2019"),
         season_seeded = stringr::str_to_title(season_seeded)) %>%
  select(siteID, numID, sampling_year, n_strips, species_seeded, age_yrs, hectares_in_strips,
         avg_p_a, season_seeded) %>%
  filter(!(is.na(numID)))

table1 <- site_info_tab %>%
  arrange(numID) %>%
  gt(rowname_col = "numID")%>%
  tab_stubhead(label = "ID") %>%
  fmt_number(columns = vars(hectares_in_strips, avg_p_a),
             decimals = 2) %>%
  cols_label(sampling_year = "Sampling year",
             species_seeded = "Seed mix richness",
             age_yrs = "Age (years)",
             hectares_in_strips = "Size (ha)",
             avg_p_a = "P:A ratio",
             season_seeded = "Season planted") %>%
  tab_options(stub.font.weight = "bold",
              column_labels.font.size = "18px",
              column_labels.border.bottom.width = 4,
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "black",
              table_body.vlines.width = 1,
              table_body.vlines.style = "solid",
              table_body.vlines.color = "black",
              table_body.hlines.color = "black",
              column_labels.vlines.style = "solid",
              column_labels.vlines.width = 1,
              column_labels.vlines.color = "black",
              stub.border.color = "black",
              table_body.border.bottom.color = "black") %>%
  cols_align(align = "center") %>%
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(vars(species_seeded),
                                   rows = is.na(species_seeded))) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_stub(rows = TRUE))

table1

gtsave(table1, filename = "site_info.png", path = "data-raw/JoAE_manuscript/manu_figs")

        
# 
# Table 1 - Diversity Table -------------------------------------------------------------

# Attempting to make this table in gt
data("site_div_rich")

# gamma diversity model
gmod <- lmer(gamma_div ~ year + species_seeded + log(hectares_in_strips) + 
             (1|siteID), data = site_div_rich)

gci <- confint.merMod(gmod) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable")

gtab <- as.data.frame(coef(summary(gmod))) %>%
  tibble::rownames_to_column(var = "variable") %>%
  left_join(., gci) %>%
  mutate_at(vars(`2.5 %`, `97.5 %`), ~round(., digits = 2)) %>%
  tidyr::unite(col = "CIs", `2.5 %`, `97.5 %`, sep = ", ") %>%
  select(variable, "g_est" = Estimate, "g_ci" = CIs, "g_p" = `Pr(>|t|)`)

# beta diversity model
bmod <- lmer(beta_div ~ year + species_seeded + 
             (1|siteID), data = site_div_rich)
bci <- confint.merMod(bmod) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable")

btab <- as.data.frame(coef(summary(bmod))) %>%
  tibble::rownames_to_column(var = "variable") %>%
  left_join(., bci) %>%
  mutate_at(vars(`2.5 %`, `97.5 %`), ~round(., digits = 2)) %>%
  tidyr::unite(col = "CIs", `2.5 %`, `97.5 %`, sep = ", ") %>%
  select(variable, "b_est" = Estimate, "b_ci" = CIs, "b_p" = `Pr(>|t|)`)

# alpha diversity model 
data("quad_div_rich")

amod <- lmer(alpha_div ~ year + species_seeded + log(hectares_in_strips) +  
             age_yrs +
             (1|siteID) + (1|quadratID:siteID), data = quad_div_rich)
aci <- confint.merMod(amod) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "variable")

atab <- as.data.frame(coef(summary(amod))) %>%
  tibble::rownames_to_column(var = "variable") %>%
  left_join(., aci) %>%
  mutate_at(vars(`2.5 %`, `97.5 %`), ~round(., digits = 2)) %>%
  tidyr::unite(col = "CIs", `2.5 %`, `97.5 %`, sep = ", ") %>%
  select(variable, "a_est" = Estimate, "a_ci" = CIs, "a_p" = `Pr(>|t|)`)

# full join these?

div_tab <- 
  full_join(gtab, btab, by = "variable") %>%
  full_join(atab, by = "variable") %>%
  tibble::add_row(variable = "Perimeter: area ratio") %>%
  tibble::add_row(variable = "Season planted") %>%
  slice(2:n()) %>%
  tibble::add_row(variable = "Site", g_p = 0.0000001, b_p = 0.0000001,
                  a_p = 0.0000001) %>%
  tibble::add_row(variable = "Quadrat:Site", a_p = 0.000001) %>%
  tibble::add_row(variable = "Marginal", g_ci = "0.56", b_ci = "0.31", a_ci = "0.2") %>%
  tibble::add_row(variable = "Conditional", g_ci = "0.88", b_ci = "0.85", a_ci = "0.45") %>%
  mutate(variable = recode(variable, "year2019" = "Sampling year",
                           "species_seeded" = "Seed mix richness",
                           "log(hectares_in_strips)" = "Size (ha)",
                           "age_yrs" = "Age")) 
# Making into gt table
table2 <- div_tab %>%
  gt(rowname_col = "variable") %>%
  cols_label(g_est = "Est.",
             g_ci  = "95% CI",
             g_p   = "P",
             b_est = "Est.",
             b_ci  = "95% CI",
             b_p   = "P",
             a_est = "Est.",
             a_ci  = "95% CI",
             a_p   = "P",) %>%
  tab_spanner(label = html("&#947"), columns = c("g_est", "g_ci", "g_p")) %>%
  tab_spanner(label = html("&#946"), columns = c("b_est", "b_ci", "b_p")) %>%
  tab_spanner(label = html("&#945"), columns = c("a_est", "a_ci", "a_p")) %>%
  fmt_number(columns = ends_with("est"),
             decimals = 2) %>%
  fmt_number(columns = ends_with("p"), decimals = 3) %>%
  text_transform(locations = cells_body(columns = vars(a_p),
                                         rows = c(3:4, 7:8)), 
                  fn = function(x){
                         ifelse(x < 0.001, "<0.001", x)
                              } ) %>%
  text_transform(locations = cells_body(columns = vars(g_p, b_p),
                                        rows = 7), 
                 fn = function(x){
                   ifelse(x < 0.001, "<0.001", x)
                 } ) %>%
  fmt_missing(columns = everything(),
              rows = 1:8,
              missing_text = "---") %>%
  fmt_missing(columns = everything(),
              rows = 9:10,
              missing_text = "") %>%
  cols_align(align = "center") %>%
  tab_row_group(group = "Fixed effects",
                rows = 1:6) %>%
  tab_row_group(group = "Random effects",
                rows = 7:8) %>%
  tab_row_group(group = "R-squared",
                rows = 9:10) %>%
  row_group_order(groups = c("Fixed effects", "Random effects", "R-squared")) %>%
  tab_footnote(footnote = "Sampling year is a categorical variable with two levels (2018 & 2019). 
               Therefore, estimates reported are the effect of 2019 relative to 2018. ",
               locations = cells_stub(rows = "Sampling year")) %>%
  tab_footnote(footnote = "Size and perimeter: area ratio were log-transformed in the model. 
               Estimates are the log transformed. In text estimates are calculated by taking β1*log(2).",
               locations = cells_stub(rows = c("Size (ha)", "Perimeter: area ratio"))) %>%
  tab_options(row_group.background.color = "lightgrey",
              column_labels.border.top.color = "white",
              column_labels.border.bottom.color = "black",
              stub.border.color = "black",
              table.border.bottom.color = "black",
              row_group.border.bottom.color = "black",
              row_group.border.top.color = "black",
              table_body.border.bottom.color = "black") %>%
  tab_style(cell_text(size = 16, weight = "bold"), 
            locations = cells_column_spanners(spanners = everything())) %>%
  tab_style(cell_text(style = "italic"),
            locations = cells_column_labels(columns = everything())) %>%
  tab_style(cell_borders(sides = "right", color = "black", weight = 10),
            locations = cells_body(columns = vars(g_p, b_p),
                                   rows = everything()))
table2

gtsave(table2, filename = "tab2_div_models.png")

# Figure S6 - season vs relative cover of prairie species across all sites ----------

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
  ggtitle("C. Forbs")+
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
  ggtitle("B. Grasses")+
  labs(x = NULL, 
       y = NULL)+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 15))
p3

season_vs_relcov <- p1 + p3 + p2

ggsave("season_vs_relCov_all.png", plot = season_vs_relcov, dpi = 600, 
       width= 9, height = 4.5)

# Figure S7 - prairie vs weedy species cover
data("veg_mid")
nat_codes <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^prairie")) %>%
  select(speciesID) %>%
  unlist()
weeds <- species_list %>% # 5 letter codes for weedy species
  filter(stringr::str_detect(group, "^weedy")) %>%
  select(speciesID) %>%
  unlist()

p_cov <- veg_mid %>%
  select(year, siteID, one_of(nat_codes)) %>%
  mutate(p_cov = rowSums(.[, 3:ncol(.)])) %>%
  group_by(year, siteID) %>%
  summarize(avg_p_cov = mean(p_cov))

w_cov <- veg_mid %>%
  select(year, siteID, one_of(weeds)) %>%
  mutate(w_cov = rowSums(.[, 3:ncol(.)])) %>%
  group_by(year, siteID) %>%
  summarize(avg_w_cov = mean(w_cov))

veg_cov <- full_join(p_cov, w_cov)


# model
l1 <- lmerTest::lmer(avg_p_cov ~ year+avg_w_cov + (1|siteID), veg_cov)
summary(l1)
ggResidpanel::resid_panel(l1)
performance::r2(l1)
lmerTest::rand(l1)   # site doesn't explain much variation

# plot
slps <- data.frame(year = c("2018", "2019"),
                   ints = c(114.18585, (114.18585 - 1.40410)),
                   slps = c(-0.66562, -0.66562))

veg_cov %>%
  ggplot(aes(avg_w_cov, avg_p_cov)) +
  geom_point(size = 2)+
  facet_wrap(~year)+
  geom_abline(data = slps, aes(slope = slps, intercept = ints),
              lty = 2)+
  labs(x = "Average weedy cover \nper quadrat",
       y = "Average prairie cover \nper quadrat")+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15))

ggsave("pr_vs_wd_cov.png", dpi = 600, width = 7, height = 5.5)
