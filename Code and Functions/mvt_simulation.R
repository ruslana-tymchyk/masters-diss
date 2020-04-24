library(tidyverse)
library(data.table)
library(rtdists)
library(afex)
library(emmeans)
library(dplyr)
library(truncdist)
library(BayesFactor)
set_sum_contrasts()
source("mvt_sim_functions.R")
library(parallel)
library(tmvtnorm)
theme_set(theme_bw())
#-----------------------------------------------------------------------------------
#------MVT--------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#Create df for simulation-------
#mu <- c(2.8, 0.2, 1, 0.3, -0.6, 0.50)
#mu: an,st0, sv,t0n, v, z
load("sigma.rda")
combos <- expand.grid(0:8, seq(0, 4, by = 0.5))
combos_all <- combos %>% mutate(an = rep(2.8, nrow(combos)), 
                                 st0 = rep(0.2, nrow(combos)),
                                 sv = combos$Var2,       
                                 t0n = rep(0.3, nrow(combos)),
                                 v = combos$Var1, 
                                 z = rep(0.5, nrow(combos)))
combos_all$Var1 <- NULL
combos_all$Var2 <- NULL
colnames(combos_all) <- NULL
combos_use <- t(split(t(combos_all), rep(1:ncol(t(combos_all)), each = nrow(t(combos_all)))))

#Run simulation-------
sim_mvt_100t_10k <- mcmapply(reruns, mu = combos_use,
                                    MoreArgs = list(sigma = sigma, 
                                                    pp = 30,
                                                    n = 100,
                                                    runs = 10000), mc.cores = 11)
sim_mvt_100t_10k <- as.data.frame(sim_mvt_100t_10k)
sim_mvt_100t_10k <- do.call(rbind.data.frame, sim_mvt_100t_10k)
rownames(sim_mvt_100t_10k) <- NULL
save(sim_mvt_100t_10k, file = "sim_mvt_100t_10k.rda")
#save(sim_mvt_100t_1k, file = "sim_mvt_100t_1k.rda")
#sim_mvt_1t_10K DONE
#sim_mvt_100t_1k - multivariate with 100 trials per participant DONE
#-----------------------------------------------------------------------------------
#------PLOTS sim_mvt_1t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
#Load data--------
load("sim_mvt_1t_10K.rda")
#Plot of v against T1-------
same_vs <- sim_mvt_1t_10K %>% 
  group_by(v_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

same_vs2 <- same_vs %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))

ggplot(same_vs2, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point(aes(shape = analysis), size = 2.5) + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT 1 trial per participant') +
  scale_x_continuous(name="Mean of v") +
  scale_y_continuous(name="Type 1 error rate") +
  theme_bw()
#Plot of sv against T1---------
same_svs <- sim_mvt_1t_10K %>% 
  group_by(sv_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

same_svs2 <- same_svs %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))

ggplot(same_svs2, aes(y = T1_error_rate, x = sv_mean, colour = analysis)) +
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT: 1 trial per participant') +
  scale_x_continuous(name="Mean of sv") +
  scale_y_continuous(name="Type 1 error rate") +
  geom_point() + 
  theme_bw()

#Plot of diff_props with cuts----------
sim_vsv3 <- sim_mvt_1t_10K %>% mutate(diff_props = abs(diff_props))
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
  ggtitle('MVT:1 trial per participant') + 
  scale_y_continuous("Type 1 error rate") +
  scale_x_discrete("Difference in proportion correct") +
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw()
#The plot of diffs, no diffs-------
diff_params <- sim_mvt_1t_10K %>%
  mutate(diffs = ifelse(abs(diff_props) < 0.05, 
                        'no_diff', 'yes_diff')) %>% 
  group_by(v_mean, sv_mean, diffs) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(diff_params)
diff_params <- diff_params %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(4:5))
ggplot(diff_params, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point() + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT: 100 trials per participant,only when there is no supposed difference in proportions') +
  theme_bw() +
  facet_grid(sv_mean ~ diffs)

#Mean of groups against T1----------
extreme_responses <- sim_mvt_1t_10K %>% mutate(extreme = ifelse(mean_prop_real < 0.1, 'less_than_10',
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

#The plot of v, faceted by sv--------
diff_params <- sim_mvt_1t_10K %>% 
  group_by(v_mean, sv_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

diff_params <- diff_params %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(3:4))

ggplot(diff_params, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point() + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  scale_x_discrete(name = "Location of V") +
  scale_y_continuous(name = "Type 1 error rate") +
  ggtitle('MVT: 1 trial per participant') +
  theme_bw() +
  facet_grid(~sv_mean)

#-----------------------------------------------------------------------------------
#------PLOTS sim_mvt_100t_1k-----------------------------------------------------------
#-----------------------------------------------------------------------------------
#Load data--------
load("sim_mvt_100t_1k.rda")
#Plot of v against T1-------
same_vs <- sim_mvt_100t_1k %>% 
  group_by(v_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

same_vs2 <- same_vs %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))

ggplot(same_vs2, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point(aes(shape = analysis), size = 2.5) + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT 100 trials per participant') +
  scale_x_continuous(name="Mean of v") +
  scale_y_continuous(name="Type 1 error rate") +
  theme_bw()
#Plot of sv against T1---------
same_svs <- sim_mvt_100t_1k %>% 
  group_by(sv_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

same_svs2 <- same_svs %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:3))

ggplot(same_svs2, aes(y = T1_error_rate, x = sv_mean, colour = analysis)) +
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT: 100 trials per participant') +
  scale_x_continuous(name="Mean of sv") +
  scale_y_continuous(name="Type 1 error rate") +
  geom_point() + 
  theme_bw()

#Plot of diff_props with cuts----------
sim_vsv3 <- sim_mvt_100t_1k %>% mutate(diff_props = abs(diff_props))
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
  ggtitle('MVT:100 trials per participant') + 
  scale_y_continuous("Type 1 error rate") +
  scale_x_discrete("Difference in proportion correct") +
  geom_hline(yintercept=0.05, linetype="dashed") +
  theme_bw()
#The plot of diffs, no diffs-------
diff_params <- sim_mvt_100t_1k %>%
  mutate(diffs = ifelse(abs(diff_props) < 0.05, 
                        'no_diff', 'yes_diff')) %>% 
  group_by(v_mean, sv_mean, diffs) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())
head(diff_params)
diff_params <- diff_params %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(4:5))
ggplot(diff_params, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point() + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  ggtitle('MVT: 100 trials per participant,only when there is no supposed difference in proportions') +
  theme_bw() +
  facet_grid(sv_mean ~ diffs)

#Mean of groups against T1----------
extreme_responses <- sim_mvt_100t_1k %>% mutate(extreme = ifelse(mean_prop_real < 0.1, 'less_than_10',
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

#The plot of v, faceted by sv--------
diff_params <- sim_mvt_100t_1k %>% 
  group_by(v_mean, sv_mean) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

diff_params <- diff_params %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(3:4))

ggplot(diff_params, aes(y = T1_error_rate, x = v_mean, colour = analysis)) +
  geom_point() + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  scale_x_discrete(name = "Location of V") +
  scale_y_continuous(name = "Type 1 error rate") +
  ggtitle('MVT: 100 trials per participant') +
  theme_bw() +
  facet_grid(~sv_mean)
