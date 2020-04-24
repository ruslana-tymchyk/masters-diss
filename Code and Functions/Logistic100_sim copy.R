source("Logistic100_sim_fun.R")

rerun_logistic3(n = 60, size = 100, reruns = 5) #check if works
#-----------------------------------------------------------------------------------
#------sim_log_100t_10K-----------------------------------------------------------
#-----------------------------------------------------------------------------------
sim_log_100t_10K <- mcmapply(rerun_logistic3,n = 60, 
                                             size = 100, 
                                             reruns = 10000, 
                                            mc.cores = 11)
sim_log_100t_10K <- as.data.frame(sim_log_100t_10K)
sim_log_100t_10K <- do.call(rbind.data.frame, sim_log_100t_10K)
rownames(sim_log_100t_10K) <- NULL
save(sim_log_100t_10K, file = "sim_log_100t_10K.rda")
