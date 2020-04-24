#Simulate just one rbinom 
#rbinom(n, size, prob)
#n:number of observaitons AKA participants
#size: number of trials AKA per participant
#p: probability of success on each trial 

#Do the run and analysis for one dataset only, then rerun 10K times
run_logistic_once <- function(n, size, prob) {
  g1 <- rbinom(n/2, size, prob)
  g2 <- rbinom(n/2, size, prob)
  #Here prob is the same for both groups and for all participants
  #The result of rbinom is the NUMBER of successful trials out of all the trials 
  gg <- cbind(g1, g2)
  gg <- as.data.frame(gg)
  g1_prop <- mean(gg$g1)
  g2_prop <- mean(gg$g2)
  diff_props <- g1_prop - g2_prop
  mean_prop_real <- (g1_prop+g2_prop)/2
  gg <- gather(gg, group, response, g1:g2, factor_key=TRUE)
  gg$id <- seq.int(nrow(gg))
  #Then analyse with Anova and GLM 
  if (mean_prop_real == 1) {
    aov_p = 1}
  else if (mean_prop_real == 0) {
    aov_p = 1}
  else {
   aov_ss <- aov_ez(
      id = "id",
      dv = "response",
      data = gg,
      between = "group",
      fun_aggregate = mean
    ) #runs anova
  aov_p <- summary(aov_ss)[["Pr(>F)"]][[1]] #extracting p-value
  }
  gg_for_glm <- gg %>% 
    group_by(id,group) %>% 
    summarise(resp_prop = mean(response), 
              n_trials = n()) %>% 
    ungroup %>% 
    mutate(group = factor(group))
  browser()
  glm_gg <- glm(
    resp_prop ~ group,
    data = gg_for_glm,
    weights = n_trials,
    family = binomial
  ) #runs glm
  glm_anova <- car::Anova(glm_gg, type = 3)
  glm_p <- glm_anova$`Pr(>Chisq)`  #extracting p-value
  glmm_run <- glmer(
    resp_prop ~ group + (1|id),
    data = gg_for_glm,
    weights = n_trials,
    family = binomial
  )
  glmm_anova <- car::Anova(glmm_run, type = 3)
  glmm <- glmm_anova$`Pr(>Chisq)`  #extracting p-value
  data <-
    tibble(
      aov_p = aov_p,
      glm_p = glm_p,
      glmm = glmm,
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

rerun_logistic <- function(n, size, prob, reruns) {
  result <- rerun(reruns, 
                  run_logistic_once(n, size, prob)) %>% map_dfr(., as_tibble)
  return(as.data.frame(result))
}
