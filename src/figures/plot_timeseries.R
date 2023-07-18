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

#### TIME SERIES - DISTURBANCE, MAKE MATTERS WORSE -----------------------------
# setup 
data = list() 
data[[1]] = IC
# implement management disturbance on initial conditions
data[[1]] <- disturb(data[[1]], worse_dist_locs[1], worse_dist_locs[2], c(1,1), "N1")

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

#### TIME SERIES - DISTURBANCE, ACHIEVE OBJECTIVE ------------------------------

# setup 
data = list() 
data[[1]] = IC

# implement management disturbance on initial conditions
data[[1]] <- disturb(data[[1]], achieve_dist_locs[1], achieve_dist_locs[2], c(1,1), "N1")

# run simulations until all have reached carrying capacity in all cells
t = 1
n_sim = nrow(data$N1)
for(t in 1:25){
  data[[t+1]] = run_single_timestep(data[[t]], Param.FireAnt.nodist, disp_prob)
  # update time counter
  t = t + 1
}

# restructure for plotting
ts_dist2 <- lapply(data, function(i){list(N1 = apply(i$N1, 2, mean), 
                                          N2 = apply(i$N2, 2, mean))})
ts_dist2_N1 <- do.call(rbind, lapply(ts_dist2, function(i){i$N1}))
ts_dist2_N1 <- reshape2::melt(ts_dist2_N1) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N1")
ts_dist2_N2 <- do.call(rbind, lapply(ts_dist2, function(i){i$N2}))
ts_dist2_N2 <- reshape2::melt(ts_dist2_N2) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N2")
ts_dist2 = bind_rows(ts_dist2_N1, 
                     ts_dist2_N2)
rm(ts_dist2_N1)
rm(ts_dist2_N2)

#### TIME SERIES - DISTURBANCE, WASTE EFFORT -----------------------------------

# setup 
data = list() 
data[[1]] = IC

# implement management disturbance on initial conditions
data[[1]] <- disturb(data[[1]], waste_dist_locs[1], waste_dist_locs[2], c(1,1), "N1")

# run simulations until all have reached carrying capacity in all cells
t = 1
n_sim = nrow(data$N1)
for(t in 1:25){
  data[[t+1]] = run_single_timestep(data[[t]], Param.FireAnt.nodist, disp_prob)
  # update time counter
  t = t + 1
}

# restructure for plotting
ts_dist3 <- lapply(data, function(i){list(N1 = apply(i$N1, 2, mean), 
                                          N2 = apply(i$N2, 2, mean))})
ts_dist3_N1 <- do.call(rbind, lapply(ts_dist3, function(i){i$N1}))
ts_dist3_N1 <- reshape2::melt(ts_dist3_N1) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N1")
ts_dist3_N2 <- do.call(rbind, lapply(ts_dist3, function(i){i$N2}))
ts_dist3_N2 <- reshape2::melt(ts_dist3_N2) %>%
  setnames("Var1", "time") %>%
  setnames("Var2", "location") %>%
  mutate(species = "N2")
ts_dist3 = bind_rows(ts_dist3_N1, 
                     ts_dist3_N2)
rm(ts_dist3_N1)
rm(ts_dist3_N2)


#### PLOT OUTCOMES -------------------------------------------------------------
plt = bind_rows(
  ts_no_dist %>% mutate(type = "no disturbance"), 
  ts_dist %>% mutate(type = "disturbance_worse"), 
  ts_dist2 %>% mutate(type = "disturbance_achieve"),
  ts_dist3 %>% mutate(type = "disturbance_waste")
)

# find wave front
detection_thresh = 100
detection_location = calc_wave_front(IC$N1, detection_thresh)[1]
wave_actual = calc_wave_front(IC$N1, 1)[1]
x_lims = c(-32, 32)

species_labels = data.frame(x = c(x_lims[1]+1,x_lims[2]-1)+wave_actual, 
                            y = c(max(IC$N1[1,]) - 600, max(IC$N2[1,])+600), 
                            text = c("invader", "native"), 
                            hjust = c(0,1), 
                            time = 1)

t_labs <- paste("year", c(1,10,25))
names(t_labs) <- c(1,10,25)

p_achieve <- plt %>%
  filter(time %in% c(1,10, 25), 
         # location < 50, 
         # location > 10, 
         type %in% c("no disturbance", "disturbance_achieve")) %>%
  ggplot() +
  geom_rect(data = data.frame(xmin = achieve_dist_locs[1]+wave_actual, 
                              xmax = achieve_dist_locs[2]+wave_actual, 
                              ymin = 0, ymax = Inf, 
                              time = 1), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin,  ymax = ymax), fill = "lightgrey", alpha = 0.4) +
  geom_line(aes(x = location, y = value, color = species, linetype = type)) + 
  geom_text(data = data.frame(x = mean(achieve_dist_locs)+wave_actual, 
                              y = 10000, 
                              label = "pesticides\napplied", 
                              time = 1), 
            aes(x = x, y = y, label = label), vjust = 0.95, size = 1.6) +
  geom_text(data = data.frame(x = 50, 
                              y = 10000, 
                              label = "achieve objective", 
                              time = 25), 
            aes(x = x, y = y, label = label), vjust = 0.95, size = 2.5, color = brewer.pal(6, "PuOr")[5]) +
  geom_text(data = species_labels, aes(x = x, y = y, label = text, hjust = hjust), size = 2.5) +
  facet_grid(cols = vars(time), 
             labeller = labeller(time = t_labs)) + 
  labs(x = "location on landscape", y = "density") +
  scale_color_manual(values = c("darkgrey", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = wave_actual,
                     expand = c(0,0), 
                     labels = "wave\nfront",
                     limits = x_lims+wave_actual, 
                     name = "location on landscape")+
  scale_y_continuous(name = "density") +
  theme_bw() +
  theme(axis.text.y =element_blank(), # axis.text.y
        axis.ticks.y = element_blank(), #  axis.ticks.y
        axis.title.x = element_blank(),
        #axis.title.y = element_text(angle = 0, vjust =0.95),
        legend.position = "none",
        panel.grid = element_blank(), 
        strip.background = element_blank())

p_waste <- plt %>%
  filter(time %in% c(1,10, 25), 
         # location < 50, 
         # location > 10, 
         type %in% c("no disturbance", "disturbance_waste")) %>%
  ggplot() +
  geom_rect(data = data.frame(xmin = waste_dist_locs[1]+wave_actual, 
                              xmax = waste_dist_locs[2]+wave_actual, 
                              ymin = 0, ymax = Inf, 
                              time = 1), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin,  ymax = ymax), fill = "lightgrey", alpha = 0.4) +
  geom_line(aes(x = location, y = value, color = species, linetype = type)) + 
  geom_text(data = data.frame(x = 50, 
                              y = 10000, 
                              label = "waste effort", 
                              time = 25), 
            aes(x = x, y = y, label = label), vjust = 0.95, size = 2.5, color = "grey") +
  geom_text(data = species_labels, aes(x = x, y = y, label = text, hjust = hjust), size = 2.5) +
  facet_grid(cols = vars(time), 
             labeller = labeller(time = t_labs)) + 
  labs(x = "location on landscape", y = "density") +
  scale_color_manual(values = c("darkgrey", "black")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = wave_actual,
                     expand = c(0,0), 
                     labels = "wave\nfront",
                     limits = x_lims+wave_actual, 
                     name = "location on landscape")+
  scale_y_continuous(name = "density") +
  theme_bw() +
  theme(axis.text.y =element_blank(), # axis.text.y
        axis.ticks.y = element_blank(), #  axis.ticks.y
        axis.title.x = element_blank(),
        #axis.title.y = element_text(angle = 0, vjust =0.95),
        legend.position = "none",
        panel.grid = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_blank())

p_worse <- plt %>%
  filter(time %in% c(1,10, 25), 
         # location < 50, 
         # location > 10, 
         type %in% c("no disturbance", "disturbance_worse")) %>%
  ggplot() +
  geom_rect(data = data.frame(xmin = worse_dist_locs[1]+wave_actual, 
                              xmax = worse_dist_locs[2]+wave_actual, 
                              ymin = 0, ymax = Inf, 
                              time = 1), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin,  ymax = ymax), fill = "lightgrey", alpha = 0.4) +
  geom_line(aes(x = location, y = value, color = species, linetype = type)) + 
  geom_text(data = species_labels, aes(x = x, y = y, label = text, hjust = hjust), size = 2.5) +
  geom_text(data = data.frame(x = 50, 
                              y = 10000, 
                              label = "make matters worse", 
                              time = 25), 
            aes(x = x, y = y, label = label), vjust = 0.95, size = 2.5, color = brewer.pal(6, "PuOr")[2]) +
  guides(color = "none") +
  facet_grid(cols = vars(time), 
             labeller = labeller(time = t_labs)) + 
  scale_color_manual(values = c("darkgrey", "black")) +
  scale_linetype_manual(values = c("dashed", "solid"), 
                        labels = c("with pesticide", "without pesticide")) +
  scale_x_continuous(breaks = wave_actual,
                     expand = c(0,0), 
                     labels = "wave\nfront",
                     limits = x_lims+wave_actual, 
                     name = "location on landscape")+
  scale_y_continuous(name = "density") +
  theme_bw() +
  theme(axis.text.y =element_blank(), # axis.text.y
        axis.ticks.y = element_blank(), #  axis.ticks.y
        #axis.title.y = element_text(angle = 0, vjust =0.95),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank(), 
        strip.text = element_blank())

l <- get_legend(p_worse)


plot_grid(p_achieve, 
          p_waste,
          p_worse +
            theme(legend.position = "none"), 
          l, 
          ncol = 1, rel_heights = c(0.32, 0.3, 0.3, 0.08), 
          labels = LETTERS[1:3])

ggsave("output/figures/timeseries_both.pdf", width = 18, height = 18, unit = "cm")