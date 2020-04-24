source("Logistic_sim_fun.R")
library(MASS)
library(visdat)
library(tidyverse)
library(data.table)
library(readxl)
library(rtdists)
library(xtable)
library(afex)
library(emmeans)
library(dplyr)
library(fitdistrplus)
library(truncdist)
#-----------------------------------------------------------------------------------
#------sim_log_1t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
probs <- seq(0.5, 0.975, by = 0.05)
sim_log_1t_10K <- mcmapply(rerun_logistic, prob = probs,
                            MoreArgs = list(n = 60, 
                                            size = 1, 
                                            reruns = 10000), 
                                             mc.cores = 11)
sim_log_1t_10K <- as.data.frame(sim_log_1t_10K)
sim_log_1t_10K <- do.call(rbind.data.frame, sim_log_1t_10K)
rownames(sim_log_1t_10K) <- NULL
save(sim_log_1t_10K, file = "sim_log_1t_10K.rda")
sim_log_1t_10K
#-----------------------------------------------------------------------------------
#------PLOTS sim_log_1t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
#Load data--------
load("sim_log_1t_10K.rda")
#Plot of diff_props with cuts----------
sim_vsv3 <- sim_log_1t_10K %>% mutate(diff_props = abs(diff_props))
cut_data <- sim_vsv3 %>% mutate(cuts = cut(sim_vsv3$diff_props,breaks = seq(0,0.25,by = 0.02)))

plot_cuts <- cut_data %>% 
  group_by(cuts) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n(),
            n = n())
plot_cuts <- plot_cuts %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))
plot_cuts %>% 
  filter(!is.na(cuts)) %>% 
  ggplot(aes(x = cuts,colour = analysis, shape = analysis)) +
  scale_shape_manual(values=c(0,1)) +
  geom_point(aes(y = T1_error_rate, size = n)) + 
  scale_size(guide = 'none') +
  ggtitle('Binom:1 trial per participant') + 
  scale_y_continuous("Type 1 error rate") +
  scale_x_discrete("Difference in proportion correct") +
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw()
#The plot of diffs, no diffs-------
diff_params <- sim_log_1t_10K %>%
  mutate(diffs = ifelse(abs(diff_props) < 0.05, 
                        'no_diff', 'yes_diff')) %>% 
  group_by(diffs) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(diff_params)

#Mean of groups against T1----------
extreme_responses <- sim_log_1t_10K %>% mutate(extreme = ifelse(mean_prop_real < 0.1, 'less_than_10',
                                                                ifelse(mean_prop_real > 0.9, 
                                                                       "more_than_90", 'other')))

extreme_plot <- extreme_responses %>% 
  group_by(extreme) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(extreme_plot)
extreme_plot <- extreme_plot %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))
extreme_plot %>% 
  ggplot(aes(x = extreme)) +
  geom_point(aes(y = T1_error_rate)) + 
  ggtitle('Binom: 1 trial per participant, extreme response proportions') + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw() +
  facet_wrap(~analysis)
#-----------------------------------------------------------------------------------
#------PLOTS sim_log_1t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
#Load data--------
load("sim_log_1t_10K.rda")
#Plot of diff_props with cuts----------
sim_vsv3 <- sim_log_1t_10K %>% mutate(diff_props = abs(diff_props))
cut_data <- sim_vsv3 %>% mutate(cuts = cut(sim_vsv3$diff_props,breaks = seq(0,0.25,by = 0.02)))

plot_cuts <- cut_data %>% 
  group_by(cuts) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n(),
            n = n())
plot_cuts <- plot_cuts %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))
plot_cuts %>% 
  filter(!is.na(cuts)) %>% 
  ggplot(aes(x = cuts,colour = analysis, shape = analysis)) +
  scale_shape_manual(values=c(0,1)) +
  geom_point(aes(y = T1_error_rate, size = n)) + 
  scale_size(guide = 'none') +
  ggtitle('Binom:1 trial per participant') + 
  scale_y_continuous("Type 1 error rate") +
  scale_x_discrete("Difference in proportion correct") +
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw()
#The plot of diffs, no diffs-------
diff_params <- sim_log_1t_10K %>%
  mutate(diffs = ifelse(abs(diff_props) < 0.05, 
                        'no_diff', 'yes_diff')) %>% 
  group_by(diffs) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(diff_params)

#Mean of groups against T1----------
extreme_responses <- sim_log_1t_10K %>% mutate(extreme = ifelse(mean_prop_real < 0.1, 'less_than_10',
                                                                ifelse(mean_prop_real > 0.9, 
                                                                       "more_than_90", 'other')))

extreme_plot <- extreme_responses %>% 
  group_by(extreme) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(extreme_plot)
extreme_plot <- extreme_plot %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))
extreme_plot %>% 
  ggplot(aes(x = extreme)) +
  geom_point(aes(y = T1_error_rate)) + 
  ggtitle('MVT: 100 trials per participant, extreme response proportions') + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw() +
  facet_wrap(~analysis)
#-----------------------------------------------------------------------------------
#------sim_log_1t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
check <- rerun(60, rbinom(1, 100, rnorm(1, 0.5, 0.2)))
View(check)

check_df <- as.data.frame(check)

