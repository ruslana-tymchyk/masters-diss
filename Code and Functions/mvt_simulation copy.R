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
