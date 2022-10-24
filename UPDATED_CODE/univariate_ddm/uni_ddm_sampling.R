#-------------------------------------
#----DESCRIPTION: RM & BS Sampling----
#-------------------------------------
#Distributions: Normal & T (truncated or not)
#Analysis: GLM & ANOVA (optional - Bayesian Anova)
#Design: Within-Subjects (Repeated Measures) + Between-Subjects

#This set of function can be used to simulate and analyse the datasets using Drift Diffusion Model.
#The data is being sampled from a distribution that best fits the given parameter 
#The sampled parameters are then passed to rdiffusion to generate the data 
#The data is then analysed using GLM and ANOVA

#-----------------Defining Distributions - identical for BS & RM-------
#This code is required for the t-distribution, as the t-distribution specified 
#in R does not permit to set location and scale parameters
dmyt <- function(x, location, scale, df) {
  1/scale * dt((x - location)/scale, df)
}
pmyt <- function(q, location, scale, df) {
  pt((q-location)/scale, df)
}

qmyt <- function(p, location, scale, df) {
  qt(p,df)*scale + location
}
rmyt <- function(n, location, scale, df) {
  (rt(n, df)*scale) + location
}

#Function that defines a truncated normal distribution
gen_norm <- function(par, a, b) {
  rtrunc(
    1,
    spec = "norm",
    a = a, #lower limit
    b = b, #upper limit
    mean = par[1],
    sd = par[2]
  )
}

#Function that defines a truncated T-distribution
gen_myt <- function(par, a, b) {
  rtrunc(
    1,
    spec = "myt",
    a = a, #lower limit
    b = b, #upper limit
    df = par[1],
    location = par[2],
    scale = par[3]
  )
}

#-----------------Sampling the parameter values-------
#Function accepts the summary statistics of each paramter as an argument and 
#returns the vector of parameter values sampled from a best-fit distribution
par_from_val <-  function(a, z, st0, v, t0, sv) {
  c(
    #defining limits of truncation and distribution for each parameter
    a = gen_norm(par = a, a = 0, b = Inf),
    z = gen_norm(par = z, a = 0, b = 1),
    st0 = unlist(gen_norm(
      par = st0, a = 0, b = Inf
    )),
    v = unlist(gen_myt(
      par = v,
      a = -10 + v[2],
      b = 10 - v[2]
      #addition and substraction of location parameter required 
      #to ensure than the upper and lower thresholds deviate by 10 
      #points from the central point of distribution rather than 
      #from zero 
    )),
    t0 = unlist(gen_myt(
      par = t0, a = 0, b = Inf
    )),
    sv = unlist(gen_myt(
      par = sv, a = 0, b = 10
    ))
  )
}

#-----------------Simulating the data for BS-------
simulate_dt_bs <- function(a, z, st0, v, t0, sv, n, pp, group) {
  params <- function(a, z, st0, v, t0, sv) {
    repeat{
      values <- par_from_val(a, z, st0, v, t0, sv)
      trials <- rdiffusion(
        n = n,
        v = values[["v"]],
        a = values[["a"]],
        t0 = values[["t0"]],
        sv = values[["sv"]],
        st0 = values[["st0"]],
        z = values[["z"]],
        stop_on_error = FALSE
      ) #simulates n trials for 1 participant
      if (mean(trials$rt) != 0) {break} 
    }
    #when combination of sampled values produces error in rdiffusion, the 
    #values are resampled and the simulation is run again
    return(trials)
  }
  result <- rerun(pp, params(a, z, st0, v, t0, sv)) %>% #simulating for pp number of participants
    rbindlist(., idcol = TRUE) %>% #adds id column
    mutate(group = rep(group))  #adds group number
  result <- result %>% rename("id" = ".id") %>% 
    mutate(id = paste0(group, "_", id)) #adding participant identifier
  return(result)
}

#-----------------Simulating the RM data-------
simulate_dt_rm <- function(a, z, st0, v, t0, sv, n, pp) {
  params <- function(a, z, st0, v, t0, sv) {
    repeat{
      #in repeated measures within subject factor is a frame
      #by simulating 2 types of trials here, we are simulating a frame
      values <- par_from_val(a, z, st0, v, t0, sv)
      trials1 <- rdiffusion(
        n = n/2,
        v = values[["v"]],
        a = values[["a"]],
        t0 = values[["t0"]],
        sv = values[["sv"]],
        st0 = values[["st0"]],
        z = values[["z"]],
        stop_on_error = FALSE
      ) 
      trials2 <- rdiffusion(
        n = n/2,
        v = values[["v"]],
        a = values[["a"]],
        t0 = values[["t0"]],
        sv = values[["sv"]],
        st0 = values[["st0"]],
        z = values[["z"]],
        stop_on_error = FALSE
      ) 
      #HENRIK - how would it be different if we simulated these data 
      #once together and split it into two frames (gain and loss)???
      #given there is no difference in how we simulate gain & loss frames
      trials1 <- trials1 %>% mutate(frame = 1)
      trials2 <- trials2 %>% mutate(frame = 2)
      trials <- rbind(trials1, trials2)
      if (mean(trials$rt) != 0) {break}
      #when combination of sampled values produces error in rdiffusion, the 
      #values are resampled and the simulation is run again
    }
    return(trials)
  }
  result <- rerun(pp, params(a, z, st0, v, t0, sv)) %>%
    rbindlist(., idcol = TRUE)  %>% #adds id column 
    rename("id" = ".id") 
  return(result)
}
