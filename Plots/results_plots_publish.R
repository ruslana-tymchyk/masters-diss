#----preparing the datasets-------
#load("sim_vsv_1t_10K.rda")
#load("sim_vsv_100t_1K.rda")
load("sim_log_1t_10K.rda")
load("sim_log_100t_10K.rda")
load("sim_mvt_1t_10K.rda")
sim_mvt_1t_10K <- sim_mvt_1t_10K %>% rename(v_loc = v_mean, sv_loc = sv_mean)
load("sim_mvt_100t_10k.rda")
sim_log_100t_10K <- sim_log_100t_10K_latest #renaming back, as using new dataset 
sim_mvt_100t_10k <- sim_mvt_100t_10k %>% rename(v_loc = v_mean, sv_loc = sv_mean)
x <- 1:nrow(sim_log_100t_10K)
div_2 <- function(x) x[x %% 2 == 0]
sim_log_100t_10K <- sim_log_100t_10K[ c(div_2(x)), ]

View(sim_log_1t_10K)
View(sim_log_100t_10K)
View(sim_mvt_1t_10K)
View(sim_mvt_100t_10k)
#load("sim_rm_200t_1K.rda")
#------load this-------
library(tidyverse)
library(afex)
library(data.table)
library(extrafont)
theme_set(theme_bw())
#setwd("~/Desktop/vsv_mvt_01Aug")
load("all_data.rda")
#------importing all the rms-------
#------not publishing them---------
#load("sim_rm_10t_10K_1.rda")
#load("sim_rm_10t_10K_2.rda")
#load("sim_rm_10t_10K_3.rda")
#load("sim_rm_10t_10K_4.rda")
#load("sim_rm_10t_10K_5.rda")
#load("sim_rm_10t_10K_6.rda")
#load("sim_rm_10t_10K_7.rda")
#load("sim_rm_10t_10K_8.rda")
#load("sim_rm_10t_10K_9.rda")
#load("sim_rm_10t_10K_10.rda")
#sim_rm_10t_10K <- rbindlist(list(sim_rm_10t_10K_1,sim_rm_10t_10K_2,
#                            sim_rm_10t_10K_3,sim_rm_10t_10K_4,sim_rm_10t_10K_5,
#                            sim_rm_10t_10K_6, sim_rm_10t_10K_7, sim_rm_10t_10K_8, 
#                            sim_rm_10t_10K_9, sim_rm_10t_10K_10))
#sim_rm_10t_10K <- na.omit(sim_rm_10t_10K)
#------creating all data with rms-------
all_data <- rbindlist(list(logistic1 = sim_log_1t_10K, logistic100 = sim_log_100t_10K, ddm1 = sim_mvt_1t_10K, ddm100 = sim_mvt_100t_1k), use.names = TRUE, idcol = "file", fill = TRUE)

#all_data <- all_data %>% filter(v_loc <= 5)
all_data$file <- as.factor(all_data$file)
str(all_data$file)
#Reordering factors
print(levels(all_data$file))  ## This will show the levels of x are "Levels: a b c d e"
## To reorder the levels:
## note, if x is not a factor use levels(factor(x))
all_data$file = factor(all_data$file,levels(all_data$file)[c(1,3,6,2,4,5)])
#-----------------------------------------------------------------------------------
#------Summary plot: all sims-----------------------------------------------------------
#-----------------------------------------------------------------------------------
all_sims_plot <- all_data %>% 
  group_by(file) %>%
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            GLMM = sum(glmm <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n())

all_sims_plot <- all_sims_plot %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(2:4))

pdf('all_plots_results.pdf',width=7,height=4)
ggplot(all_sims_plot, aes(y = T1_error_rate, x = file, colour = analysis)) +
  geom_point(aes(shape = analysis), size = 4) + 
  geom_hline(yintercept=0.05, linetype="dashed") +
  scale_x_discrete(name="Simulation") +
  scale_y_continuous(name="Type I error rate") +
  scale_shape_discrete(name="Analysis",
                       labels=c("ANOVA","GLM", "GLMM")) +
  scale_color_manual(values=c("#E7B800","#00AFBB","#D95F02"),
                     name="Analysis",
                     labels=c("ANOVA","GLM", "GLMM")) +
  theme(text=element_text(family="LM Roman 10", size=15),
        #panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
dev.off()
#-----------------------------------------------------------------------------------
#------Summary plot:plot_cuts-----------------------------------------------------------
#-----------------------------------------------------------------------------------
all_data_cuts <- all_data %>% mutate(diff_props = abs(diff_props))
cut_data <- all_data_cuts %>% 
  mutate(cuts = cut(all_data_cuts$diff_props,breaks = seq(0,0.2,by = 0.02), 
                    labels = c(seq(0.02,0.2,by = 0.02))))
plot_cuts <- cut_data %>% 
  group_by(cuts,file) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            GLMM = sum(glmm <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n(), 
            n = n())
plot_cuts <- plot_cuts %>% mutate(n = ifelse(file == 'mvt100' | 
                                                     file == 'bs100' , n*10, n))
plot_cuts <- plot_cuts %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(3:5))
plot_cuts <- na.omit(plot_cuts)
#plot_cuts$cuts <- as.factor(plot_cuts$cuts)
pdf('plot_cuts_all_results.pdf',width=12,height=7)
plot_cuts %>% 
  filter(!is.na(cuts)) %>% 
  ggplot(aes(x = cuts,colour = analysis, shape = analysis)) +
  scale_shape_manual(values=c(0,1)) +
  geom_point(aes(y = T1_error_rate, size = n)) + 
  scale_size(guide = 'none') +
  geom_hline(yintercept=0.05, linetype="dashed") +
  scale_y_continuous("Proportion of p < 0.05") +
  scale_x_discrete("Range of differences in response proportion") +
  scale_shape_discrete(name="Analysis",
                       labels=c("ANOVA","GLM", "GLMM")) +
  scale_color_manual(values=c("#E7B800","#00AFBB","#D95F02"),
                     name="Analysis",
                     labels=c("ANOVA","GLM", "GLMM")) +
  theme(text=element_text(family="LM Roman 10", size=15)) +
        #panel.grid.major = element_blank()) +
  facet_wrap(~file, nrow = 2)
dev.off()
#-----------------------------------------------------------------------------------
#------Summary plot:mean_prop vs T1-----------------------------------------------------------
#-----------------------------------------------------------------------------------
extreme_responses <- all_data %>% mutate(extreme = ifelse(mean_prop_real > 0.9, 'More than 90', "Less than 90"))

check <- extreme_responses %>% 
  mutate(diff_props = abs(diff_props)) %>% 
  filter(diff_props <= 0.02)

View(check)

extreme_plot <- check %>% 
  group_by(extreme,file) %>% 
  summarise(GLM = sum(glm_p <= 0.05)/n(),
            GLMM = sum(glmm <= 0.05)/n(),
            ANOVA = sum(aov_p <= 0.05)/n(),
            n = n())
extreme_plot <- extreme_plot %>% mutate(n = ifelse(file == 'mvt100' | 
                                                     file == 'bs100' , n*10, n))

extreme_plot <- extreme_plot %>% gather(.,key = 'analysis', value = 'T1_error_rate', c(3:5))
pdf('extreme_plot_results.pdf',width=12,height=5)
extreme_plot %>% 
  ggplot(aes(x = extreme, colour = analysis, shape = analysis)) +
  geom_point(aes(y = T1_error_rate, size = n)) + 
  scale_size_area() +
  guides(size = FALSE) +
  scale_x_discrete(name = "Response proportion") +
  geom_hline(yintercept=0.05, linetype="dashed") +
  scale_y_continuous("Proportion of p < 0.05") +
  scale_shape_discrete(name="Analysis",
                       labels=c("ANOVA","GLM", "GLMM")) +
  scale_color_manual(values=c("#E7B800","#00AFBB","#D95F02"),
                     name="Analysis",
                     labels=c("ANOVA","GLM", "GLMM")) +
  theme(text=element_text(family="LM Roman 10", size=15),
        panel.grid.major = element_blank()) + 
  facet_wrap(~file, nrow =2)
dev.off()
