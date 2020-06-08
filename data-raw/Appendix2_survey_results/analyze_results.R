# Looking at survey results
library(dplyr)
library(ggplot2)
library(patchwork)         # for putting plots together
theme_set(theme_bw())
library(extrafont)
init_surv  <- readr::read_csv("data-raw/Appendix2_survey_results/initial_survey_results.csv")
final_surv <- readr::read_csv("data-raw/Appendix2_survey_results/final_survey_results.csv")

# How many people answered the survey?

init_resp <- 
  init_surv %>%
  filter(!(is.na(grassNum))) %>%
  mutate(surv = "Initial")
final_resp <- 
  final_surv %>%
  filter(!(is.na(grassNum))) %>%
  mutate(surv = "Final")

# join the surveys

all_surv <- 
  bind_rows(init_resp, final_resp) %>%
  arrange(survey_code) %>%
  mutate(surv = forcats::as_factor(surv))

# Question 1. Number of grasses species identified
# change in grass id
grass_change <- 
  all_surv %>%
  group_by(survey_code) %>%
  mutate(change = grassNum - lag(grassNum, default = grassNum[1])) %>%
  select(survey_code, surv, grassNum, change) %>%
  filter(surv == "Final") %>%
  mutate(change_grass = if(change > 0){
    change2 <- "increase" 
  }else if (change < 0) {
    change2 <- "decrease"
  }else{
    change2 <- "same"
  }) %>%
  select(survey_code, change_grass)

q1 <- all_surv %>%
  left_join(grass_change) %>%
  ggplot(aes(surv, grassNum))+
  geom_point(size = 3)+
  geom_line(aes(group = survey_code, color = change_grass), size = 1.25)+
  labs(x = NULL,
       y = NULL)+
  ggtitle("A. Grasses")+
  guides(color = FALSE)+
  scale_color_manual(values = c("#8FB440", "#6C7258", "grey"))+
  scale_y_continuous(breaks = c(0, 5, 10, 15))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        title = element_text(size = 16)) 
q1 
# Question 2. Number of forbs sepcies identified
forb_change <- 
  all_surv %>%
  group_by(survey_code) %>%
  mutate(change = forbNum - lag(forbNum, default = forbNum[1])) %>%
  select(survey_code, surv, forbNum, change) %>%
  filter(surv == "Final") %>%
  mutate(change_forb = if(change > 0){
    change_forb <- "increase" 
  }else if (change < 0) {
    change_forb <- "decrease"
  }else{
    change_forb <- "same"
  }) %>%
  select(survey_code, change_forb)

q2 <- 
  all_surv %>%
  left_join(forb_change) %>%
  ggplot(aes(surv, forbNum))+
  geom_point(size = 3)+
  geom_line(aes(group = survey_code, color = change_forb), size = 1.25)+
  labs(x = NULL,
       y = NULL)+
  ggtitle("B. Forbs")+
  guides(color = FALSE)+
  scale_color_manual(values = c("#8FB440", "#6C7258", "grey"))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        title = element_text(size = 16))
q2
q1 + q2

# Question 3 - interest in prairie species identication

q3 <- 
  all_surv %>%
  ggplot(aes(interestScale))+
  geom_histogram(binwidth = 0.5, fill = "grey", color = "black")+
  labs(x = "Interest Scale",
       y = "# of Respondents")+
  facet_wrap(~surv)+
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))+
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 14),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 15))
q3

# Question 4 - methods to identify plants

q4_df <- 
  all_surv %>%
  tidyr::pivot_longer(cols = c(stratA:stratE), names_to = "strategy_type", values_to = "pref") %>%
  select(survey_code, surv, strategies, strategy_type, pref) 

q4 <- 
  q4_df %>%
  ggplot(aes(strategy_type, pref))+
  geom_col(fill = "grey50")+
  labs(x = NULL,
       y = "# of Respondents")+
  scale_x_discrete(labels = c("Collect samples", 
                              "Take photos",
                              "Use a guidebook",
                              "Consult someone else",
                              "Consult internet"))+
  facet_wrap(~surv)+
  theme(axis.text  = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.4)),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = rel(1.5)))+
  coord_flip()
q4

# Question 5. Favorite prairie plant

favs <- 
  all_surv %>%
  select(survey_code, surv, favorite)
favs

# Question 6. Looking at change in ID
 
final_resp %>%
  group_by(idChange) %>%
  count()

q6 <- data.frame(resp = c("Increased", "Stayed the same", "decreased"),
                 n    = c(13, 3, 0))

ggplot(q6, aes(resp, n))+
  geom_col(fill = "grey", color = "black")+
  scale_x_discrete(limits = c("Increased", "Stayed the same", "Decreased"))+
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12))+
  labs(x = NULL, y = "# of Respondents")+
  theme(axis.text = element_text(size = 12, family = "Fira Sans"),
        axis.title = element_text(size = 14, family = "Fira Sans"))
