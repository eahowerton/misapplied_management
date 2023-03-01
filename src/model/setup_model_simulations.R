nCol = 60
n.samples = 500
Param.FireAnt = list(a11 = 1/10000,   a12 = 1/3000-0.000025, r1 = 1.2, Disp1 = 0.2, DispDist1 = 5,
                     a22 = 1/3000,   a21 = 1/10000 + 0.000025, r2 = 1.2, Disp2 = 0.2, DispDist2 = 2,
                     Dist1 = 1, Dist2 = 1, DistColStart = -10, DistColEnd = 12, dist_type = "N1")
# find carrying capacity
Param.FireAnt["K1"] = ifelse((Param.FireAnt[["a22"]] > Param.FireAnt[["a12"]] & Param.FireAnt[["a11"]] > Param.FireAnt[["a21"]]) | (Param.FireAnt[["a22"]] == Param.FireAnt[["a12"]] & Param.FireAnt[["a11"]] == Param.FireAnt[["a21"]]),
                             (Param.FireAnt[["a22"]] - Param.FireAnt[["a12"]])/((Param.FireAnt[["a11"]]*Param.FireAnt[["a22"]]) - (Param.FireAnt[["a12"]]*Param.FireAnt[["a21"]])), 
                             1/Param.FireAnt[["a11"]])
# find transition probability matrices
disp_prob = list()
disp_prob[[1]] = create_disp_mat(n_col = nCol , 
                                 max_disp_dist = Param.FireAnt[['DispDist1']],
                                 dist_pct = Param.FireAnt[["Disp1"]])
disp_prob[[2]] = create_disp_mat(n_col = nCol , 
                                 max_disp_dist = Param.FireAnt[['DispDist2']],
                                 dist_pct = Param.FireAnt[["Disp2"]])

# generate initial conditions
# run simulation until N1 reaches the center of the arena (col 30)
t = 1
data = list("N1" = matrix(c(0.1/Param.FireAnt[["a11"]],rep(0, times = nCol-1)), nrow = 1, ncol = nCol),
            "N2" =matrix(1/Param.FireAnt[["a22"]], nrow = 1, ncol = nCol))
Param.FireAnt.nodist = Param.FireAnt
Param.FireAnt.nodist[["Dist1"]] = 0
Param.FireAnt.nodist[["Dist2"]] = 0
while(calc_wave_front(data$N1) < 30 & t < 501){
  data = run_single_timestep(data, Param.FireAnt.nodist, disp_prob)
  # update time counter
  t = t + 1
}
IC = lapply(data, function(i){return(matrix(rep(i, n.samples), 
                                            nrow = n.samples,
                                            byrow = TRUE))})


#### SETUP DISTURBANCE LOCATIONS TO TEST ---------------------------------------
# collect in data frame
testing = expand.grid(DistColStart =  seq(-30,2, by = 2), 
                      DistColEnd = seq(-30,30, by = 2), 
                      Dist1 = 1, Dist2 = 1)
testing = rbind(testing, expand.grid(DistColStart = 1, DistColEnd = 1, Dist1 = 0, Dist2 = 0))
wave = calc_wave_front(IC$N1)[1]
# remove testing rows that do not make sense --- REVISIT THIS
testing = testing[testing$DistColEnd < (nCol - wave),]
testing = testing[abs(testing$DistColStart) < wave,]
testing = testing[testing$DistColStart <= testing$DistColEnd,]
testing = testing[testing$DistColEnd < 10, ]
testing$id = 1:nrow(testing)



