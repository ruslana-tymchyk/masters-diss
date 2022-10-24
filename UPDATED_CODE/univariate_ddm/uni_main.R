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
#directory where you want to save the files
wd <- "/Users/ruslanatymchyk/Documents/MSc Behavioural Economics/Dissertation/masters-diss/UPDATED_CODE/simulated_data"
#-----------------------------------------------------------
# distribution: uni, multi
# model: log, ddm
# design: bs, rm
# n: number of participants
# size: number or trials per participant
# reruns: number of datasets to be simulated
name_dataframe <- function(wd, distribution, model, design, n, size, reruns){
  properties <- c(wd, "/",
                  distribution, "_", 
                  model, "_", 
                  design, "_", 
                  n, "pp", 
                  size, "tr", reruns, "runs.rda")
  #each property has to be a string
  newname <- paste(properties, collapse = "")
  #newname <- oldname #make new dataset point to an old one
  print(newname)
  newname
}

#OPTIONS: reruns_rm OR reruns_bs, depending on the experiment you want to run
#-------------------------------------------------------------------------
#------RM & BS - identical code for both---------------------------------------------------------
#-------------------------------------------------------------------------
##Create lists that contain all combinations of values for simulations------
simulate_uni_data <- function(model, pp, n, runs){
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
  #produce 81 datasets, one for each combination of v and sv
  #Run simulation-------
  sim_rm <- mapply(model, v = v_sim,
                   sv = sv_sim,
                   MoreArgs = list(a = c(2.8, 0.8),
                                   z = c(0.5, 0.1),
                                   st0 = c(0.2, 0.1),
                                   t0 = c(5.2, 0.3, 0.2),
                                   pp = pp,
                                   n = n,
                                   runs = runs))
  #Code below turns the output into dataframe
  make_df <- function(df){
    df <- as.data.frame(df)
    df <- do.call(rbind.data.frame, df)
    #rownames(df) <- NULL
  }
  sim_rm_final <- make_df(sim_rm)
  file_name <- name_dataframe(wd = wd,
                              distribution = 'uni', 
                              model = 'ddm', 
                              design = 'bs', 
                              n = pp, 
                              size = n, 
                              reruns = runs)
  save(sim_rm_final, file = file_name)
}

