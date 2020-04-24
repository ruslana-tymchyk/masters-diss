#rbinom(n, size, prob)
#n:number of observaitons AKA participants
#size: number of trials AKA per participant
#p: probability of success on each trial 

one_participant <- function(size, id) {
  repeat
  {prob <- rnorm(1, 0.5, 0.2)
  if (is.na(prob) == FALSE & prob <= 1) break}
  rbinom_mine <- function(prob){
    repeat
    {rbin <- rbinom(1, 1, abs(prob))
    if (is.na(rbin) == FALSE) 
      break}
    rbin
  }
  correct <- rerun(size, rbinom_mine(prob))
  correct <- as.data.frame(correct)
  probs <- as.data.frame(prob)
  participant <- rep(id, size)
  probs <- rep(abs(prob), size)
  output <- rbind(participant,probs,correct)
  return(output)
}

many_participants <-function(n, size){
  l <- vector("list", n)
  i <- 1
  while(i < n+1) {
    l[[i]] <- one_participant(size = size, id = i)
    i <- i + 1
  }
  l
}

run_logistic_once <- function(n, size) {
  g1 <- many_participants(n, size)
  g1 <- transpose(do.call(cbind, g1))
  g1 <- as.data.frame(g1)
  g1$group <- rep('g1', nrow(g1))
  setnames(g1, old=c("V1","V2","V3"), new=c("id", "prob", "response"))
  #g1$prob <- unlist(g1$prob)
  #g1$response <- unlist(g1$response)
  
  g2 <- many_participants(n, size)
  g2 <- transpose(do.call(cbind, g2))
  g2 <- as.data.frame(g2)
  g2$group <- rep('g2', nrow(g2))
  setnames(g2, old=c("V1","V2","V3"), new=c("id", "prob", "response"))
  g2 <- g2 %>% mutate(id = id+length(g2) + 100)
  g1_prop <- mean(g1$response)
  g2_prop <- mean(g2$response)
  g1_probab <- mean(g1$prob)
  g2_probab <- mean(g2$prob)
  diff_probabilities <- g1_probab - g2_probab
  diff_props <- g1_prop - g2_prop
  mean_prop_real <- (g1_prop+g2_prop)/2
  gg <- rbind(g1,g2)
  #gg$id <- seq.int(nrow(gg))
  #Then analyse with Anova and GLM  
  if(mean_prop_real == 1) {
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
  glmm <- glmm_anova$`Pr(>Chisq)`[2]  #extracting p-value
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
      g1_probab = g1_probab,
      g2_probab = g2_probab,
      diff_probabilities = diff_probabilities
    )
  data
}

rerun_logistic3 <- function(n, size, reruns) {
  result <- rerun(reruns, 
                  run_logistic_once(n, size)) %>% map_dfr(., as_tibble)
  return(as.data.frame(result))
}