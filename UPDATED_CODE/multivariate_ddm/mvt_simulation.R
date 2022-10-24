#--------------------------------------------------------------------------
#------------------------------DESCRIPTION---------------------------------
#--------------------------------------------------------------------------
#Distributions: Normal & T (truncated or not)
#Analysis: GLM & ANOVA 
#Design: Between-Subjects & Repeated Measures

#This set of functions can be used to simulate the datasets using Drift Diffusion Model.
#The data is being sampled from the multivariate normal distribution, to account for 
#correlations between the parameters
#The sampled parameters are then passed to rdiffusion to generate the data 
#The data is then analysed using GLM and ANOVA

#import functions for sampling
source("mvt_ddm_sampling.R")
#import functions for analysis
source("mvt_analysis.R")
#-----------BS Simulation---------------

#-----------------Simulating the data + analysing---------------------------
#Analysing 1 dataset with 'pp' participants and 'size' number of trials 
data_analysis_bs <- function(mu,sigma,size,pp) {
  #------------Simulate Data for Groups 1 & 2-----------------
  s1 <- simulate_dt_bs(
    mu = mu,
    sigma = sigma,
    size = size,
    pp = pp,
    group = 1  #adds column with a group number
  ) 
  pp_g1 <- as.numeric(as.character(summarise(s1, n_distinct(id)))) #number of pp's
  s2 <- simulate_dt_bs(
    mu = mu, 
    sigma = sigma, 
    size = size,
    pp = pp,
    group = 2
  )
  pp_g2 <- as.numeric(as.character(summarise(s2, n_distinct(id))))
  #------------Prepare for analysis with ANOVA-----------------
  ss <- rbind(s1, s2) %>%  #binds simulated datasets together
    mutate(response = ifelse(response == "upper", 1, 0))  #transforms response into numeric
    mean_prop <- ss %>%
    group_by(group) %>%
    summarise(diff_props = mean(response)) #proportion of upper responses
  g1_prop <- mean_prop$diff_props[1] #proportion of upper for group 1
  g2_prop <- mean_prop$diff_props[2] #proportion of upper for group 2
  mean_prop_real <- ss %>%
    summarise(diff_props = mean(response))
  mean_prop_real <- as.numeric(mean_prop_real) #proportion of upper responses
  #for both groups combined
  diff_props <- g1_prop - g2_prop #difference in proportion
  #------------Analyse with ANOVA-----------------
  aov_p <- anova_for_mvt_bs(mean_prop_real, ss)
  #------------Prepare for analysis with GLM-----------------
  ss_for_glm <- ss %>% 
    group_by(id,group) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(group = factor(group))
  #------------Analyse with GLM-----------------
  glm_p <- glm_for_mvt_bs(ss_for_glm)
  #------------Return results of analysis & summary statistics-----------------
  data <-
    tibble(
      aov_p = aov_p,
      glm_p = glm_p,
      diff_props = diff_props,
      mean_prop_real = mean_prop_real,
      g1_prop = g1_prop,
      g2_prop = g2_prop,
      size = size,
      pp_g1 = pp_g1,
      pp_g2 = pp_g2,
      a_mean = mu[1],
      a_sd = sqrt(sigma[1,1]/mu[1]),
      st0_mean = mu[2],
      st_sd = sqrt(sigma[2,2]/mu[2]),
      sv_mean = mu[3],
      sv_sd = sqrt(sigma[3,3]/1),
      t0_mean = mu[4],
      t0_sd = sqrt(sigma[4,4]/mu[4]),
      v_mean = mu[5],
      v_sd = sqrt(sigma[5,5]/0.25),
      z_mean = mu[6],
      z_sd = sqrt(sigma[6,6]/mu[6])
    ) #produces list with all the values required for further analysis
  data
}

reruns_bs <- function(mu,sigma,size,pp,reruns) {
  result <- rerun(reruns,
                  data_analysis_bs(mu, sigma,size,pp)) %>% 
                  map_dfr(., as_tibble)
  return(as.data.frame(result))
}

#-----------------Simulating the RM data-------
# So we only need to simulate one group here
#Such that each participant in this group has been exposed to an 
#equal number of trials in loss and gain frames

data_analysis_rm <- function(mu,sigma,size,pp) {
  #------------Simulate Data for all participants-----------------
  ss <- simulate_dt_rm(
    mu = mu,
    sigma = sigma,
    size = size,
    pp = pp
  ) 
  #------------Prepare for analysis with ANOVA-----------------
  ss <- ss %>%
    mutate(response = ifelse(response == "upper", 1, 0))  #transforms response into numeric
  mean_prop <- ss %>%
    group_by(frame) %>%
    summarise(diff_props = mean(response)) #proportion of upper responses
  frame1_prop <- mean_prop$diff_props[1] #proportion of upper for group 1
  frame2_prop <- mean_prop$diff_props[2] #proportion of upper for group 2
  mean_prop_real <- ss %>%
    summarise(diff_props = mean(response))
  mean_prop_real <- as.numeric(mean_prop_real) #proportion of upper responses
  #for both groups combined
  diff_props <- frame1_prop - frame2_prop #difference in proportion
  #------------Analyse with ANOVA-----------------
  aov_p <- anova_for_mvt_rm(mean_prop_real, ss)
  #------------Prepare for analysis with GLM-----------------
  gg_for_glm <- ss %>% 
    group_by(id,frame) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(frame = factor(frame))
  #------------Analyse with GLM-----------------
  glmm_p <- glmm_for_mvt_rm(mean_prop_real,
                          frame1_prop, 
                          frame2_prop, 
                          gg_for_glm)
  #------------Return results of analysis & summary statistics-----------------
  data <-
    tibble(
      aov_p = aov_p,
      glmm_p = glmm_p,
      diff_props = diff_props,
      mean_prop_real = mean_prop_real,
      frame1_prop = frame1_prop,
      frame2_prop = frame2_prop,
      size = size,
      pp = pp,
      a_mean = mu[1],
      a_sd = sqrt(sigma[1,1]/mu[1]),
      st0_mean = mu[2],
      st_sd = sqrt(sigma[2,2]/mu[2]),
      sv_mean = mu[3],
      sv_sd = sqrt(sigma[3,3]/1),
      t0_mean = mu[4],
      t0_sd = sqrt(sigma[4,4]/mu[4]),
      v_mean = mu[5],
      v_sd = sqrt(sigma[5,5]/0.25),
      z_mean = mu[6],
      z_sd = sqrt(sigma[6,6]/mu[6])
    ) #produces list with all the values required for further analysis
  data
}

reruns_rm <- function(mu,sigma,size,pp,reruns) {
  result <- rerun(reruns,
                  data_analysis_rm(mu, sigma,size,pp)) %>% 
    map_dfr(., as_tibble)
  return(as.data.frame(result))
}
