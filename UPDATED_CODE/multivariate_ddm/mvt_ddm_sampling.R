#-------Sampling the parameter values - identical for BS & RM-------------------------
#Function accepts the vector of means(mu) and covariance matrix(sigma) of 
#the selected parameter values and returns the vector of parameter values
#sampled from a best-fit distribution
par_from_val <- function(mu,sigma) {
  gen <- rtmvnorm(1, mean = mu, 
                  sigma = sigma,
                  lower=c(0,0,0,0,-10,0), 
                  upper=c(Inf, Inf, 10, Inf, 10, 1),
                  H = NULL)
  gen <- as.data.frame(gen)
  gen <- gen %>% 
    rename(
      a = V1,
      st0 = V2,
      sv = V3,
      t0 = V4,
      v = V5,
      z = V6)
}

#-----------------Simulating the data for BS------------
simulate_dt_bs <- function(mu, sigma, size, pp, group) {
  params <- function(mu, sigma) {
    values <- par_from_val(mu, sigma)
    trials <- rdiffusion(
      n = size,
      v = values$v,
      a = values$a,
      t0 = values$t0,
      sv = values$sv,
      st0 = values$st0,
      z = values$z,
      stop_on_error = FALSE
    ) #simulates size trials for 1 participant
  }
  repeat
  {result <- rerun(pp, params(mu, sigma)) %>%
    rbindlist(., idcol = TRUE) %>% #adds id column
    mutate(group = rep(group))  #adds group number
  result <- result %>% rename("id" = ".id") %>% 
    mutate(id = paste0(group, "_", id)) %>%
    group_by(id) %>%
    filter(mean(rt) != 0) %>% 
    ungroup() #removes participants for whom rdiffusion produced an error
  if (as.numeric(as.character(summarise(result, n_distinct(id)))) == pp) break
  } #if r diffusion produced an error and there are fewer than 30 participants, 
  #reruns the simulation
  return(result)
}
#-----------------Simulating the RM data-------
simulate_dt_rm <- function(mu, sigma, size, pp) {
  params <- function(mu, sigma) {
    repeat{
    values <- par_from_val(mu, sigma)
    trials1 <- rdiffusion(
      n = size,
      v = values$v,
      a = values$a,
      t0 = values$t0,
      sv = values$sv,
      st0 = values$st0,
      z = values$z,
      stop_on_error = FALSE
    ) #simulates size trials for 1 participant
    trials2 <- rdiffusion(
      n = size,
      v = values$v,
      a = values$a,
      t0 = values$t0,
      sv = values$sv,
      st0 = values$st0,
      z = values$z,
      stop_on_error = FALSE
    ) #simulates size trials for 1 participant
    trials1 <- trials1 %>% mutate(frame = 1)
    trials2 <- trials2 %>% mutate(frame = 2)
    trials <- rbind(trials1, trials2)
    if (mean(trials$rt) != 0) {break}
    #when combination of sampled values produces error in rdiffusion, the 
    #values are resampled and the simulation is run again
    }
    return(trials)
  }
    result <- rerun(pp, params(mu, sigma)) %>%
    rbindlist(., idcol = TRUE) %>% #adds id column 
    rename("id" = ".id") 
  return(result)
}
