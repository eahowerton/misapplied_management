#### SETUP SIMULATIONS ---------------------------------------------------------
source("src/model/simulation_model.R")
source("src/model/setup_model_simulations.R")


#### SENSITIVITY ANALYSIS: NATIVE ANT PARAMS -----------------------------------
change = data.frame(changeVar = c("a22", "a22", "a22","a22",
                                  "a12", "a12", "a12","a12",
                                  "a21", "a21", "a21","a21"),
                    changeVal = c(Param.FireAnt[["a22"]] - (Param.FireAnt[["a22"]] * 0.01),
                                  Param.FireAnt[["a22"]] + (Param.FireAnt[["a22"]] * 0.01),
                                  Param.FireAnt[["a22"]] - (Param.FireAnt[["a22"]] * 0.05),
                                  Param.FireAnt[["a22"]] + (Param.FireAnt[["a22"]] * 0.05),
                                  Param.FireAnt[["a12"]] - (Param.FireAnt[["a12"]] * 0.01),
                                  Param.FireAnt[["a12"]] + (Param.FireAnt[["a12"]] * 0.01),
                                  Param.FireAnt[["a12"]] - (Param.FireAnt[["a12"]] * 0.05),
                                  Param.FireAnt[["a12"]] + (Param.FireAnt[["a12"]] * 0.05),
                                  Param.FireAnt[["a21"]] - (Param.FireAnt[["a21"]] * 0.01),
                                  Param.FireAnt[["a21"]] + (Param.FireAnt[["a21"]] * 0.01),
                                  Param.FireAnt[["a21"]] - (Param.FireAnt[["a21"]] * 0.05),
                                  Param.FireAnt[["a21"]] + (Param.FireAnt[["a21"]] * 0.05)))
change$change_id = 1:nrow(change)

t = Sys.time()
sens_results <- list()
for(j in 1:nrow(change)){
  print(j)
  param_sens = Param.FireAnt
  param_sens[[change[j,"changeVar"]]] = change[j, "changeVal"]
  sens_results[[j]] <- list()
  for(i in 1:nrow(testing)){
    sens_results[[j]][[i]] <- calc_spread_time(data = IC, 
                                          params = param_sens, 
                                          disp_prob_mat = disp_prob, 
                                          change_startcol = testing[i,"DistColStart"], 
                                          change_endcol = testing[i,"DistColEnd"], 
                                          change_distpct1 = testing[i,"Dist1"], 
                                          change_distpct2 = testing[i,"Dist2"])
  }
}
Sys.time() - t

sens <- lapply(sens_results, function(i){rbindlist(lapply(i, as.data.frame), idcol = TRUE) %>%
    setnames(".id", "id") %>% 
    setnames("X[[i]]", "invasion_time")})
sens <- rbindlist(sens, idcol = TRUE) %>%
  setnames(".id", "change_id")
sens <- sens %>% 
  left_join(change, by = "change_id")

# native dispersal distance
change2 = data.frame(changeVar = rep("DispDist2"), 
                     changeVal = c(1,3,4))
change2$change_id = 1:nrow(change2)

t = Sys.time()
sens_results2 <- list()
for(j in 1:nrow(change2)){
  print(j)
  param_sens = Param.FireAnt
  param_sens[[change2[j,"changeVar"]]] = change2[j, "changeVal"]
  disp_prob_sens = list()
  disp_prob_sens[[1]] = create_disp_mat(n_col = nCol , 
                                        max_disp_dist = param_sens[['DispDist1']],
                                        dist_pct = param_sens[["Disp1"]])
  disp_prob_sens[[2]] = create_disp_mat(n_col = nCol , 
                                        max_disp_dist = param_sens[['DispDist2']],
                                        dist_pct = param_sens[["Disp2"]])
  sens_results2[[j]] <- list()
  for(i in 1:nrow(testing)){
    sens_results2[[j]][[i]] <- calc_spread_time(data = IC, 
                                                params = param_sens, 
                                                disp_prob_mat = disp_prob_sens, 
                                                change_startcol = testing[i,"DistColStart"], 
                                                change_endcol = testing[i,"DistColEnd"], 
                                                change_distpct1 = testing[i,"Dist1"], 
                                                change_distpct2 = testing[i,"Dist2"])
  }
}
Sys.time() - t

sens2 <- lapply(sens_results2, function(i){rbindlist(lapply(i, as.data.frame), idcol = TRUE) %>%
    setnames(".id", "id") %>% 
    setnames("X[[i]]", "invasion_time")})
sens2 <- rbindlist(sens2, idcol = TRUE) %>%
  setnames(".id", "change_id")
sens2 <- sens2 %>% 
  left_join(change2, by = "change_id")

#### SENSITIVITY ANALYSIS: DISTURBANCE INTENSITY -------------------------------
change3 = data.frame(changeVar = "Dist1", 
                     changeVal = c(0.98, 0.95))
change3$change_id = 1:nrow(change3)

t = Sys.time()
sens_results3 <- list()
for(j in 1:nrow(change3)){
  print(j)
  sens_results3[[j]] <- list()
  for(i in 1:nrow(testing)){
    sens_results3[[j]][[i]] <- calc_spread_time(data = IC, 
                                                params = Param.FireAnt, 
                                                disp_prob_mat = disp_prob, 
                                                change_startcol = testing[i,"DistColStart"], 
                                                change_endcol = testing[i,"DistColEnd"], 
                                                change_distpct1 = ifelse(testing$Dist1[i] == 0, 0, change3[j,"changeVal"]), 
                                                change_distpct2 = ifelse(testing$Dist2[i] == 0, 0, change3[j,"changeVal"]))
  }
}
Sys.time() - t


sens3 <- lapply(sens_results3, function(i){rbindlist(lapply(i, as.data.frame), idcol = TRUE) %>%
    setnames(".id", "id") %>% 
    setnames("X[[i]]", "invasion_time")})
sens3 <- rbindlist(sens3, idcol = TRUE) %>%
  setnames(".id", "change_id")
sens3 <- sens3 %>% 
  left_join(change3, by = "change_id")


#### COMBINE AND SAVE ----------------------------------------------------------
sens_all <- rbindlist(list(sens, sens2, sens3)) %>%
  select(-change_id)
write.csv(sens_all, "output/simulations/all_sims_sensitivity.csv")

