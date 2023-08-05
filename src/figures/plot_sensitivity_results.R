#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

source("src/model/simulation_model.R")
source("src/model/setup_model_simulations.R")

sens_all <- read.csv("output/simulations/all_sims_sensitivity.csv")

#### CALCULATE CHANGE IN INVASION TIME -----------------------------------------
sens_all_sum <- sens_all %>%
  group_by(changeVar, changeVal, id) %>%
  summarise(mean_spread_time = mean(invasion_time))  
sens_no_control <- sens_all_sum %>%
  filter(id == which(testing$Dist1 == 0)) %>%
  rename(base_mean_spread_time = mean_spread_time) %>%
  select(-id)
sens_all_sum <- sens_all_sum %>% 
  filter(id != which(testing$Dist1 == 0)) %>%
  left_join(sens_no_control, by = c("changeVar", "changeVal")) %>%
  left_join(testing, on = "id") %>%
  mutate(delta_spread_time = mean_spread_time - base_mean_spread_time)

results = read.csv("output/simulations/all_sims_baseline.csv") %>%
  select(-X)
rsum = results %>%
  group_by(id, DistColStart, DistColEnd, Dist1, Dist2) %>%
  summarise(mean_spread_time = mean(invasion_time))
no_control = rsum %>%
  filter(Dist1 == 0)
rsum = rsum %>%
  filter(Dist1 != 0) %>%
  mutate(delta_spread_time = mean_spread_time - no_control$mean_spread_time)


plot_all <- sens_all_sum %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort %in% c(3,7,13,17,23), DistColEnd < 16)

#### CALCULATE MORTALITY RATIO -------------------------------------------------
wave_actual = calc_wave_front(IC$N1, 1)[1]
IC_shift <- data.frame(location = rep(1:ncol(IC$N1),2) - wave_actual,
                       abund = c(IC$N1[1,], IC$N2[1,]), 
                       species = c(rep("N1", ncol(IC$N1)),rep("N2", ncol(IC$N1)))) %>%
  reshape2::dcast(location ~ species, value.var = "abund")

plot_all$N1_IC_abund <- NA
plot_all$N2_IC_abund <- NA
for(i in 1:nrow(plot_all)){
  #browser()
  tmp_IC_shift <- IC_shift %>% 
    filter(location >= unlist(plot_all[i, "DistColStart"]), 
           location <= unlist(plot_all[i, "DistColEnd"]))
  plot_all[i, "N1_IC_abund"] <- sum(tmp_IC_shift$N1)
  plot_all[i, "N2_IC_abund"] <- sum(tmp_IC_shift$N2)
}

rsum$N1_IC_abund <- NA
rsum$N2_IC_abund <- NA
for(i in 1:nrow(rsum)){
  tmp_IC_shift <- IC_shift %>% 
    filter(location >= unlist(rsum[i, "DistColStart"]), 
           location <= unlist(rsum[i, "DistColEnd"]))
  rsum[i, "N1_IC_abund"] <- sum(tmp_IC_shift$N1)
  rsum[i, "N2_IC_abund"] <- sum(tmp_IC_shift$N2)
}

# filter to only points that are in plot_all
rsum <- rsum %>% 
  filter(paste(DistColStart, DistColEnd) %in% paste(plot_all$DistColStart, plot_all$DistColEnd))

#### PLOT SUMMARY RESULTS ------------------------------------------------------

labs_native = c("effect of native on invader", 
         "effect of invader on native", 
         "effect of native on native",
         "native max dispersal distance")
names(labs_native) = c("a12", "a21", "a22", "DispDist2")

change_labs <- paste("change:", c("-5%", "-1%", "1%", "5%"))
names(change_labs) <- c(-0.05,-0.01, 0.01, 0.05)

plot_all %>%
  mutate(baseVal = case_when(changeVar == "a12" ~ Param.FireAnt[["a12"]], 
                             changeVar == "a21" ~ Param.FireAnt[["a21"]], 
                             changeVar == "a22" ~ Param.FireAnt[["a22"]], 
                             changeVar == "DispDist2" ~ Param.FireAnt[["DispDist2"]], 
                             changeVar == "Dist1" ~ Param.FireAnt[["Dist1"]])) %>%
  mutate(changeValPct = round((changeVal - baseVal)/baseVal,3)) %>%
  filter(!(changeVar %in% c("Dist1", "DispDist2"))) %>%
  ggplot(aes(x = log(N1_IC_abund/N2_IC_abund), y = delta_spread_time)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) + 
  geom_point(data = rsum, color = "lightgrey") +
  geom_point() + 
  facet_grid(cols = vars(changeValPct),
             rows = vars(changeVar),
             labeller = labeller(changeVar = labs_native, 
                                 changeValPct = change_labs)) +
  scale_x_continuous(name = "mortality ratio = log(invaders killed/natives killed)") +
  scale_y_continuous(limits = c(-1,1)*max(abs(plot_all$delta_spread_time)),
                     name = "change in invasion time (years)") +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("output/figures/sensitivity_nativeparams.pdf", width = 6, height = 4, scale = 1.5)


#### MORTALITY RATE ------------------------------------------------------------
labs_mort = paste0("pesticide induced mortality: ", c("95%", "98%"))
names(labs_mort) = c(0.95, 0.98)

plot_all %>%
  mutate(baseVal = case_when(changeVar == "a12" ~ Param.FireAnt[["a12"]],
                             changeVar == "a21" ~ Param.FireAnt[["a21"]],
                             changeVar == "a22" ~ Param.FireAnt[["a22"]],
                             changeVar == "DispDist2" ~ Param.FireAnt[["DispDist2"]],
                             changeVar == "Dist1" ~ Param.FireAnt[["Dist1"]])) %>%
  mutate(changeValPct = round((changeVal - baseVal)/baseVal,3)) %>%
  filter(changeVar == "Dist1") %>%
  ggplot(aes(x = log(N1_IC_abund/N2_IC_abund), y = delta_spread_time)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point(data = rsum, color = "lightgrey") +
  geom_point() +
  facet_grid(cols = vars(changeVal),
             labeller = labeller(changeVal = labs_mort
             )) +
  scale_x_continuous(name = "mortality ratio = log(invaders killed/natives killed)") +
  scale_y_continuous(limits = c(-1,1)*max(abs(rsum$delta_spread_time)),
                     name = "change in invasion time (years)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank())
ggsave("output/figures/sensitivity_distpct_full.pdf", width = 4, height = 2.5, scale = 1.5)

#### DispDist2 -----------------------------------------------------------------
labs_disp = paste0("native max. dispersal dist: ", c("1","3","4"))
names(labs_disp) = sort(unique(sens_all_sum %>% filter(changeVar == "DispDist2") %>% pull(changeVal)))

plot_all %>%
  mutate(baseVal = case_when(changeVar == "a12" ~ Param.FireAnt[["a12"]],
                             changeVar == "a21" ~ Param.FireAnt[["a21"]],
                             changeVar == "a22" ~ Param.FireAnt[["a22"]],
                             changeVar == "DispDist2" ~ Param.FireAnt[["DispDist2"]],
                             changeVar == "Dist1" ~ Param.FireAnt[["Dist1"]])) %>%
  mutate(changeValPct = round((changeVal - baseVal)/baseVal,3)) %>%
  filter(changeVar == "DispDist2") %>%
  ggplot(aes(x = log(N1_IC_abund/N2_IC_abund), y = delta_spread_time)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point(data = rsum, color = "lightgrey") +
  geom_point() +
  facet_grid(cols = vars(changeVal),
             labeller = labeller(changeVal = labs_disp
                                 )) +
  scale_x_continuous(name = "mortality ratio = log(invaders killed/natives killed)") +
  scale_y_continuous(limits = c(-1,1)*max(abs(rsum$delta_spread_time)),
                     name = "change in invasion time (years)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank())
ggsave("output/figures/sensitivity_dispdist_full.pdf", width = 6, height = 2.5, scale = 1.5)

