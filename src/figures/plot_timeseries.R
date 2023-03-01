#### PREAMBLE
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

set.seed(76)

#### SETUP SIMULATIONS ---------------------------------------------------------
source("src/model/simulation_model.R")
source("src/model/setup_model_simulations.R")


#### TIME SERIES - NO DISTURBANCE ----------------------------------------------
# setup 
data = list() 
data[[1]] = IC

# run simulations until all have reached carrying capacity in all cells
t = 1
n_sim = nrow(data$N1)
for(t in 1:25){
  data[[t+1]] = run_single_timestep(data[[t]], Param.FireAnt.nodist, disp_prob)
  # update time counter
  t = t + 1
}

# restructure for plotting
ts_no_dist <- lapply(data, function(i){list(N1 = apply(i$N1, 2, mean), 
                                            N2 = apply(i$N2, 2, mean))})
ts_no_dist_N1 <- do.call(rbind, lapply(ts_no_dist, function(i){i$N1}))
ts_no_dist_N1 <- reshape2::melt(ts_no_dist_N1) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N1")
ts_no_dist_N2 <- do.call(rbind, lapply(ts_no_dist, function(i){i$N2}))
ts_no_dist_N2 <- reshape2::melt(ts_no_dist_N2) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N2")
ts_no_dist = bind_rows(ts_no_dist_N1, 
                       ts_no_dist_N2)
rm(ts_no_dist_N1)
rm(ts_no_dist_N2)

#### TIME SERIES - DISTURBANCE -------------------------------------------------

# setup 
data = list() 
data[[1]] = IC

# implement management disturbance on initial conditions
data[[1]] <- disturb(data[[1]], -7, 4, c(1,1), "N1")

# run simulations until all have reached carrying capacity in all cells
t = 1
n_sim = nrow(data$N1)
for(t in 1:25){
  data[[t+1]] = run_single_timestep(data[[t]], Param.FireAnt.nodist, disp_prob)
  # update time counter
  t = t + 1
}

# restructure for plotting
ts_dist <- lapply(data, function(i){list(N1 = apply(i$N1, 2, mean), 
                                            N2 = apply(i$N2, 2, mean))})
ts_dist_N1 <- do.call(rbind, lapply(ts_dist, function(i){i$N1}))
ts_dist_N1 <- reshape2::melt(ts_dist_N1) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N1")
ts_dist_N2 <- do.call(rbind, lapply(ts_dist, function(i){i$N2}))
ts_dist_N2 <- reshape2::melt(ts_dist_N2) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N2")
ts_dist = bind_rows(ts_dist_N1, 
                       ts_dist_N2)
rm(ts_dist_N1)
rm(ts_dist_N2)


#### PLOT OUTCOMES -------------------------------------------------------------
plt = bind_rows(
  ts_no_dist %>% mutate(type = "no disturbance"), 
  ts_dist %>% mutate(type = "disturbance")
)

plt %>%
  filter(time %in% c(1,10, 25), 
         location < 50, 
         location > 10) %>%
  ggplot(aes(x = location, y = value, color = species, linetype = type)) +
  geom_line() + 
  facet_grid(cols = vars(time), 
             labeller = label_both) + 
  labs(x = "location on landscape", y = "density") +
  scale_color_manual(values = c("darkgrey", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_text(angle = 0),
        legend.position = "none", 
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("output/figures/timeseries_both.pdf", width = 5.5, height = 2, scale = 1.2)


plt %>%
  filter(time %in% c(1,10, 25), 
         location < 50, 
         location > 10, 
         type == "no disturbance") %>%
  ggplot(aes(x = location, y = value, color = species, linetype = type)) +
  geom_line() + 
  facet_grid(cols = vars(time), 
             labeller = label_both) + 
  labs(x = "location on landscape", y = "density") +
  scale_color_manual(values = c("darkgrey", "black")) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_text(angle = 0),
        legend.position = "none", 
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("output/figures/timeseries_nodist.pdf", width = 5.5, height = 2, scale = 1.2)

