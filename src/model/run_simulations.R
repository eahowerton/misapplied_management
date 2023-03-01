#### PREAMBLE
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

set.seed(76)

#### SETUP SIMULATIONS ---------------------------------------------------------
source("src/model/simulation_model.R")
source("src/model/setup_model_simulations.R")


#### RUN SIMULATIONS VARYING DISTURBANCE PARAMS --------------------------------
t = Sys.time()
results <- list()
for(i in 1:nrow(testing)){
  print(i)
  results[[i]] <- calc_spread_time(data = IC, 
                                   params = Param.FireAnt, 
                                   disp_prob_mat = disp_prob, 
                                   change_startcol = testing[i,"DistColStart"], 
                                   change_endcol = testing[i,"DistColEnd"], 
                                   change_distpct1 = testing[i,"Dist1"], 
                                   change_distpct2 = testing[i,"Dist2"])
}
Sys.time() - t



# restructure results into data.frame
results <- lapply(results, as.data.frame)
results <- data.table::rbindlist(results, idcol = TRUE) %>%
  setnames(".id", "id") %>% 
  setnames("X[[i]]", "invasion_time") %>%
  left_join(testing, "id")
# save results
write.csv(results, "output/simulations/all_sims_baseline.csv")

# check no results reached maximum
which(results$invasion_time == 501)


#### RERUN SIMULATIONS WIHTOUT N2 COMPETITION ----------------------------------
IC_noN2 = IC
IC_noN2$N2 = matrix(0, nrow = nrow(IC$N2), ncol = ncol(IC$N2))

t = Sys.time()
results_noN2 <- list()
for(i in 1:nrow(testing)){
  print(i)
  results_noN2[[i]] <- calc_spread_time(data = IC_noN2, 
                                   params = Param.FireAnt, 
                                   disp_prob_mat = disp_prob, 
                                   change_startcol = testing[i,"DistColStart"], 
                                   change_endcol = testing[i,"DistColEnd"], 
                                   change_distpct1 = testing[i,"Dist1"], 
                                   change_distpct2 = testing[i,"Dist2"])
}
Sys.time() - t

# restructure results into data.frame
results_noN2 <- lapply(results_noN2, as.data.frame)
results_noN2 <- data.table::rbindlist(results_noN2, idcol = TRUE) %>%
  setnames(".id", "id") %>% 
  setnames("X[[i]]", "invasion_time") %>%
  left_join(testing, "id")
# save results
write.csv(results_noN2, "output/simulations/all_sims_noN2.csv")