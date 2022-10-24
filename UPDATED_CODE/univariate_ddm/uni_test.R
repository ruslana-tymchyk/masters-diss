library(tidyverse)
library(data.table)
library(rtdists)
library(afex)
library(truncdist)
library(parallel)
library(extrafont)
source("uni_simulation.R")
set_sum_contrasts()
theme_set(theme_bw())
#font_import(pattern = "lmroman*")
#loadfonts()
#-------------------------------------------------------------------------
#------RM & BS - identical code for both---------------------------------------------------------
#-------------------------------------------------------------------------
##Create lists that contain all combinations of values for simulations------
combos <- expand.grid(0:8, seq(0, 4, by = 0.5))
combos_v <- combos %>% mutate(df = rep(2.6, nrow(combos)), 
                              scale = rep(0.5, nrow(combos)))
combos_sv <- combos %>% mutate(df = rep(4.5, nrow(combos)), 
                               scale = rep(0.7, nrow(combos)))
v_sim <- combos_v[, c("df", "Var1", "scale")]
sv_sim <- combos_sv[, c("df", "Var2", "scale")]
colnames(v_sim) <- NULL
colnames(sv_sim) <- NULL
v_sim <- t(split(t(v_sim), rep(1:ncol(t(v_sim)), each = nrow(t(v_sim)))))
sv_sim <- t(split(t(sv_sim), rep(1:ncol(t(sv_sim)), each = nrow(t(sv_sim)))))
#Note: as there are 81 rows in v_sim and sv_sim, each run of the simulation will 
#produce 81 datasets, one for each comnination of v and sv
#Run simulation-------
sim_rm <- mapply(reruns_bs, v = v_sim,
                 sv = sv_sim,
                 MoreArgs = list(a = c(2.8, 0.8),
                                 z = c(0.5, 0.1),
                                 st0 = c(0.2, 0.1),
                                 t0 = c(5.2, 0.3, 0.2),
                                 pp = 30,
                                 n = 20,
                                 runs = 1))
#Code below turns the output into dataframe
print('just this one')
print(sim_rm)
