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
source("uni_ddm_sampling.R")
#import functions for analysis
source("uni_analysis.R")


#-----------BS Simulation---------------

#-----------------Analysing the simulated data-------
#Analysing 1 dataset with 'pp' participants and 'n' number of trials 
data_analysis_bs <- function(a,z,st0,v,t0,sv,
                             n,pp) {
  s1 <- simulate_dt_bs(
    a = a,
    z = z,
    st0 = st0,
    v = v,
    t0 = t0,
    sv = sv,
    n = n,
    pp = pp,
    group = 1  
  ) #simulating group 1
  pp_g1 <- as.numeric(as.character(summarise(s1, n_distinct(id)))) #calculates final number of pp's
  s2 <- simulate_dt_bs(
    a = a,
    z = z,
    st0 = st0,
    v = v,
    t0 = t0,
    sv = sv,
    n = n,
    pp = pp,
    group = 2
  ) #simulating group 2
  #Note: parameters for both groups are the same
  #------------Prepare for analysis with ANOVA-----------------
  pp_number <- as.numeric(as.character(summarise(s2, n_distinct(id)))) #number of participants
  ss <- rbind(s1, s2) %>%  #binds simulated datasets together
    mutate(response = ifelse(response == "upper", 1, 0))  #transforms response into numeric
  mean_prop <- ss %>%
    group_by(group) %>%
    summarise(diff_props = mean(response)) #proportion of upper responses by group
  g1_prop <- mean_prop$diff_props[1] #proportion of upper for group 1
  g2_prop <- mean_prop$diff_props[2] #proportion of upper for group 2
  mean_prop_real <- ss %>%
    summarise(props = mean(response)) 
  mean_prop_real <- as.numeric(mean_prop_real) #proportion of upper responses
  #for both groups combined
  diff_props <- g1_prop - g2_prop #difference in proportion
  #------------Analyse with ANOVA-----------------
  aov_p <- anova_for_uni_bs(mean_prop_real, ss)
  #------------Prepare for analysis with GLM-----------------
  ss_for_glm <- ss %>% 
    group_by(id,group) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(group = factor(group))
  #------------Analyse with GLM-----------------
  glm_p <- glm_for_uni_bs(ss_for_glm)
  #------------Return results of analysis & summary statistics-----------------
  data <-
    tibble(
      aov_p = aov_p,
      glm_p = glm_p,
      diff_props = diff_props,
      mean_prop_real = mean_prop_real,
      g1_prop = g1_prop,
      g2_prop = g2_prop,
      n_g1 = n,
      n_g2 = n,
      pp_number = pp_number,
      a_mean = a[1],
      a_sd = a[2],
      z_mean = z[1],
      z_sd = z[2],
      st0_mean = st0[1],
      st0_sd = st0[2],
      v_df = v[1],
      v_loc = v[2],
      v_scale = v[3],
      t0_df = t0[1],
      t0_loc = t0[2],
      t0_scale = t0[3],
      sv_df = sv[1],
      sv_loc = sv[2],
      sv_scale = sv[3]
    ) #produces list with all the values required for further analysis
  data
}
#Repeating analysis for 'runs' number of datasets
reruns_bs <- function(a,z,st0,v,t0,sv,
                      n,pp,runs) {
  result <- rerun(runs,
                  data_analysis_bs(a,z,st0,v,t0,sv,
                                   n, pp)) %>% map_dfr(., as_tibble)
  return(as.data.frame(result))
}

#-----------------Analysing the simulated RM data-------
#Analysing 1 dataset with 'pp' participants and 'n' number of trials 
data_analysis_rm <- function(a,z,st0,v,t0,sv,
                          n,pp) {
  repeat{
    ss <- simulate_dt_rm(
      a = a,
      z = z,
      st0 = st0,
      v = v,
      t0 = t0,
      sv = sv,
      n = n,
      pp = pp
    ) 
    #------------Prepare for analysis with ANOVA-----------------
    ss <- ss %>% mutate(response = ifelse(response == "upper", 1, 0))  
    #transforms response into numeric
    mean_prop_frame <- ss %>%
      group_by(frame) %>%
      summarise(diff_props = mean(response)) #proportion of upper responses
    g1_prop <- mean_prop_frame$diff_props[1] #proportion of upper for group 1
    g2_prop <- mean_prop_frame$diff_props[2] #proportion of upper for group 2
    mean_prop_real <- ss %>%
      summarise(diff_props = mean(response))
    mean_prop_real <- as.numeric(mean_prop_real)
    diff_props <- g1_prop - g2_prop #difference in proportion
    #------------Analyse with ANOVA-----------------
    aov_p <- anova_for_uni_rm(mean_prop_real, ss)
    #------------Prepare for analysis with GLM-----------------
    ss_for_glm <- ss %>% 
    group_by(id,frame) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(frame = factor(frame))
    #------------Analyse with GLM-----------------
    glmm_p <- glm_for_uni_rm(ss_for_glm, 
                            mean_prop_real,
                            g1_prop,
                            g2_prop)
    #------------Return results of analysis & summary statistics-----------------
    data <-
      tibble(
        aov_p = aov_p,
        glmm_p = glmm_p,
        sing = sing,
        diff_props = diff_props,
        mean_prop_real = mean_prop_real,
        g1_prop = g1_prop,
        g2_prop = g2_prop,
        n_g1 = n,
        n_g2 = n,
        a_mean = a[1],
        a_sd = a[2],
        z_mean = z[1],
        z_sd = z[2],
        st0_mean = st0[1],
        st0_sd = st0[2],
        v_df = v[1],
        v_loc = v[2],
        v_scale = v[3],
        t0_df = t0[1],
        t0_loc = t0[2],
        t0_scale = t0[3],
        sv_df = sv[1],
        sv_loc = sv[2],
        sv_scale = sv[3]
      ) #produces list with all the values required for further analysis
    if (data$sing == 0) {break}
    #if sing != 0 glm reached singular fit and that analysis is discarded an re-run
  } 
  data
}

#Repeating analysis for 'runs' number of datasets
reruns_rm <- function(a,z,st0,v,t0,sv,
                   n,pp,runs) {
  result <- rerun(runs,
                  data_analysis_rm(a,z,st0,v,t0,sv,
                                n,pp)
  ) %>% map_dfr(., as_tibble)
  return(as.data.frame(result))
}
