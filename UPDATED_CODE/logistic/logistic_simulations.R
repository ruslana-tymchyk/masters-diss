#Between-subjects; Logistic
#Simulate just one rbinom 
#rbinom(n, size, prob)
#n:number of observaitons AKA participants
#size: number of trials AKA per participant
#p: probability of success on each trial 
source("Logistic_analysis.R")
#---------------------------------------------------------------------------
#------------------RUN LOGISTIC BETWEEN SUBJECTS-----------------------
#---------------------------------------------------------------------------
#JP General Paper - check it out 
#linear probability model - retains linear model 
#think about transformation from linear to non linear
#what does it really mean??

#Between subjects
#Between subjects with individual differences
#Repeated Measures 
#Data has to be transformed by the inverse function.
logistic_bs_one_run <- function(n, size, prob) {
  #prob of success is the same for both groups and for all participants
  #The result of rbinom is the NUMBER of successful trials
  #------------Simulate Data for Groups 1 & 2-----------------
  g1 <- rbinom(n/2, size, prob)
  g2 <- rbinom(n/2, size, prob)
  #------------Prepare for analysis with ANOVA-----------------
  gg <- cbind(g1, g2)
  gg <- as.data.frame(gg)
  g1_prop <- mean(gg$g1)/size
  g2_prop <- mean(gg$g2)/size
  diff_props <- g1_prop - g2_prop
  mean_prop_real <- (g1_prop+g2_prop)/2
  gg <- gather(gg, group, response, g1:g2, factor_key=TRUE)
  gg$id <- seq.int(nrow(gg))
  #------------Analyse with ANOVA-----------------
  aov_p <- anova_for_logistic(mean_prop_real, gg)
  #------------Prepare for analysis with GLM & GLMM-----------------
  gg_for_glm <- gg %>% 
    group_by(id,group) %>% 
    summarise(resp_prop = mean(response)/size, 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(group = ifelse(group == "g1", 1, 2)) %>%
    mutate(group = factor(group))
  #------------Analyse with GLM-----------------
  glm_p <- glm_for_logistic(gg_for_glm)
  #------------Return results of analysis & summary statistics-----------------
  data <-
    tibble(
      aov_p = aov_p, 
      glm_p = glm_p,
      diff_props = diff_props,
      mean_prop_real = mean_prop_real,
      g1_prop = g1_prop,
      g2_prop = g2_prop,
      n = n,
      size = size,
      prob = prob
    )
  data
}

rerun_logistic_bs <- function(n, size, prob, reruns) {
  result <- rerun(reruns, 
                  logistic_bs_one_run(n, size, prob)) %>% 
    map_dfr(., as_tibble)
  return(as.data.frame(result))
}
#---------------------------------------------------------------------------
#--------------------------RUN LOGISTIC RM -----------------------------
#---------------------------------------------------------------------------
#simulate loss and gain trials for participants 
#
logistic_rm_one_run <- function(n, size, prob){
  #------------Simulate Data for Frame 1-----------------
  response <- rbinom(n, size, prob)
  frame1 <- as.data.frame(response)
  frame1$id <- seq.int(nrow(frame1))
  frame1$frame <- c(rep(1, n))
  frame1_prop <- mean(response)/size
  #------------Simulate Data for Frame 2-----------------
  response <- rbinom(n, size, prob)
  frame2 <- as.data.frame(response)
  frame2$id <- seq.int(nrow(frame2))
  frame2$frame <- c(rep(2, n))
  frame2_prop <- mean(response)/size
  #------------Prepare for analysis with ANOVA-----------------
  ff <- rbind(frame1, frame2)
  diff_props <- frame1_prop - frame2_prop
  mean_prop_real <- (frame1_prop+frame2_prop)/2
  #converting overall number of trials from logistic simulation into a rows 
  #of individual trials, so that data is in the correct format for RM analysis
  individual_response <- function(row, size){
    row <-  data.frame(as.list(row))
    responses <- strtoi(row$response)
    incorrect <- c(rep(0, size - responses))
    correct <- c(rep(1, responses))
    df <- data.frame(response = c(incorrect,  correct),
                     id = c(rep(row$id, size)),
                     frame = c(rep(row$frame, size)))
  }
  ff2 <- apply(ff,1,individual_response, size) #size is an additional argument
  #empty dataframe to append individual trials to
  final_ff <- data.frame(response = integer(),
                         id = character(),
                         frame = integer())

  for (df in ff2) {
    final_ff <- rbind(final_ff, df)
    
  }
  #------------Analyse with ANOVA-----------------
  aov_p <- anova_for_logistic_rm(mean_prop_real, final_ff)
  #------------Prepare for analysis with GLM-----------------
  ff_for_glm <- final_ff %>% 
    group_by(id,frame) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(frame = factor(frame))
  #------------Analyse with GLM-----------------
  glmm_p <- glmm_for_logistic_rm(mean_prop_real, 
                               frame1_prop, 
                               frame2_prop, 
                               ff_for_glm)
  #------------Return results of analysis & summary statistics----------------- 
  data <-
    tibble(
      aov_p = aov_p, 
      glmm_p = glmm_p,
      diff_props = diff_props,
      mean_prop_real = mean_prop_real,
      frame1_prop = frame1_prop,
      frame2_prop = frame2_prop,
      n = n,
      size = size,
      prob = prob
    )
  data
}
rerun_logistic_rm <- function(n, size, prob, reruns) {
  result <- rerun(reruns, 
                  logistic_rm_one_run(n, size, prob)) %>% 
                  map_dfr(., as_tibble)
  return(as.data.frame(result))
}
