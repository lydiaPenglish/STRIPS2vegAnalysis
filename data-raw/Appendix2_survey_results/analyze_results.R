# Looking at survey results
library(tidyverse)
library(patchwork)         # for putting plots together
theme_set(theme_bw())
init_surv  <- read_csv("data-raw/Appendix2_survey_results/initial_survey_results.csv")
final_surv <- read_csv("data-raw/Appendix2_survey_results/final_survey_results.csv")

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
  mutate(surv = as_factor(surv))

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
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.25))) 
 
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
  theme(axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.25))) 

q1 + q2

# Question 6. Looking at change in ID
 
final_resp %>%
  group_by(idChange) %>%
  count()

