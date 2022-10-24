#---------------------------------------------------------------------------
#-----------------Multivariate Between-Subjects Analysis------------------------
#---------------------------------------------------------------------------

#-------------------------Analyse with ANOVA--------------------------------
anova_for_mvt_bs <- function(mean_prop_real, ss){
    if (mean_prop_real == 1) {
    aov_p = 1}#in cases when all responses are the same, set p-value to 1
  #so that anova does not produce an error
  #particularly important when number of trials is low
  else {
    aov_ss <- aov_ez(
      id = "id",
      dv = "response",
      data = ss,
      between = "group", 
      fun_aggregate = mean
    ) #runs anova
    aov_p <- summary(aov_ss)[["Pr(>F)"]][[1]] #extracting p-value 
  }
}
#--------------------------Analyse with GLM---------------------------------

glm_for_mvt_bs <- function(ss_for_glm){
  glm_ss <- glm(
    resp_prop ~ group,
    data = ss_for_glm,
    weights = n_trials,
    family = binomial
  ) #runs glm
  glm_anova <- car::Anova(glm_ss, type = 3)
  glm_p <- glm_anova$`Pr(>Chisq)`  #extracting p-value
}

#---------------------------------------------------------------------------
#-----------------Multivariate Repeated Measures Analysis------------------------
#---------------------------------------------------------------------------

#-------------------------Analyse with ANOVA--------------------------------
anova_for_mvt_rm <- function(mean_prop_real, gg){
  if (mean_prop_real == 1 || mean_prop_real == 0) {
    aov_p = 1}
  #in cases when all responses are the same, set p-value to 1
  #so that anova does not produce an error
  #particularly important when number of trials is low
  else {
    aov_ss <- aov_ez(
      id = "id",
      dv = "response",
      data = gg,
      within = "frame", #the repeated measures factor is the within subjects factor
      fun_aggregate = mean
    ) 
    aov_p <- aov_ss$anova_table[["Pr(>F)"]][[1]] #extracting p-value
  }
  aov_p
}
#--------------------------Analyse with GLMM---------------------------------
glmm_for_mvt_rm <- function(mean_prop_real,
                                frame1_prop, 
                                frame2_prop, 
                                gg_for_glm){
  if (mean_prop_real == 1 || mean_prop_real == 0) {
    glmm_p = 1
    sing = 0}
  else if(frame1_prop == 1 & frame2_prop == 0) {
    glmm_p = 0
    sing = 1
  }
  else if(frame2_prop == 1 & frame1_prop == 0) {
    glmm_p = 0
    sing = 1
  }
  #the two 'else if' statements so there is no error due to complete separation in GLM
  else {
    glm_ss <- glmer(
      resp_prop ~ frame + (frame|id),
      data = gg_for_glm,
      weights = n_trials, 
      family = binomial
    ) 
    sing <- ifelse(isSingular(glm_ss) == TRUE, 1, 0)
    glm_anova <- car::Anova(glm_ss, type = 3)
    glmm_p <- glm_anova$`Pr(>Chisq)`[2]  #extracting p-value 
  }
  glmm_p
}  