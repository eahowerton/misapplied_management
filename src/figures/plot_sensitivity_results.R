#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

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

#### PLOT SUMMARY RESULTS ------------------------------------------------------

labs = c("effect of native on invader", 
         "effect of invader on native", 
         "effect of native on native",
         "native max dispersal distance")
names(labs) = c("a12", "a21", "a22", "DispDist2")

effort_labs = paste("effort:", c(3,7,13,17,23))
names(effort_labs) = c(3,7,13,17,23)


grad <- data.frame(x = min(plot_all$DistColStart), 
                   y = seq(-48, 
                           16, 
                           length.out = 100), 
                   effort = 3)

plot_all %>%
  mutate(baseVal = case_when(changeVar == "a12" ~ Param.FireAnt[["a12"]], 
                             changeVar == "a21" ~ Param.FireAnt[["a21"]], 
                             changeVar == "a22" ~ Param.FireAnt[["a22"]], 
                             changeVar == "DispDist2" ~ Param.FireAnt[["DispDist2"]], 
                             changeVar == "Dist1" ~ Param.FireAnt[["Dist1"]])) %>%
  mutate(changeValPct = round((changeVal - baseVal)/baseVal,3)) %>%
  filter(!(changeVar %in% c("Dist1", "DispDist2"))) %>%
  mutate(changeValPct = as.character(changeValPct)) %>%
  ggplot(aes(x = DistColStart, y = delta_spread_time)) + 
  #geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(linetype = changeValPct), size = 0.6) +
  geom_line(data = rsum %>%
              mutate(effort = DistColEnd - DistColStart + 1) %>%
              filter(effort %in% c(3,7,13,17,23), DistColEnd < 16), 
            aes(linetype = as.character(0)), size = 0.6) +
  geom_line(data = grad,
            aes(x=x, y=y, color=y),size=8)+
  facet_grid(cols = vars(effort),
             rows = vars(changeVar),
             labeller = labeller(changeVar = labs, 
                                 effort = effort_labs)) + 
  guides(color = "none") +
  labs(x = "starting location of management\n(relative to detecable invader wave front)", 
       y = "change in invasion time") +
  scale_color_distiller(direction = 0,
                        palette = "PuOr",
                        limits = c(-1,1)*max(abs(grad$y))) +
  scale_linetype_manual(breaks = as.character(c(-0.05, -0.01,0,0.01,0.05)),
                        labels = c("-5%", "-1%", "0%", "1%", "5%"),
                        values = c("twodash", "longdash", "solid", "dotted", "dashed")) +
  # scale_color_manual(breaks = as.character(c(-0.05, -0.01,0,0.01,0.05)),
  #                    labels = c("-5%", "-1%", "0%", "1%", "5%"),
  #                    values = c(brewer.pal(3,"Greens")[3:2], "black", brewer.pal(3, "Blues")[2:3])) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() + 
  theme(legend.key.width = unit(2, "lines"),
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.border = element_rect(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0.1, "lines"), 
        strip.background = element_blank())
ggsave("output/figures/sensitivity_nativeparams.pdf", width = 5.5, height = 4, scale = 1.5)


#### MORTALITY RATE ------------------------------------------------------------
labs = paste0("pesticide induced mortality: ", c("95%", "98%"))
names(labs) = c(0.95, 0.98)
p <- sens_all_sum %>%
  filter(changeVar == "Dist1") %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax),
            fill = NA, color = "grey45") +
  facet_grid(cols = vars(changeVal), 
             labeller = labeller(changeVal = labs)) +
  guides(color = "none") +
  labs(x = "ending location of management", 
       y = "starting location of management", 
       fill = "change in\ninvasion\ntime") +
  #lims(x = x_lims) +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(sens_all_sum %>% 
                                                  filter(changeVar =="Dist1") %>% 
                                                  pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    #axis.text = element_blank(),
    #axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines"), 
    strip.background = element_blank()
  )
ggsave("output/figures/sensitivity_distpct_full.pdf", p, width = 8, height = 5)


#### A12 ------------------------------------------------------------
labs = paste0("effect of native on invader: ", c("-5%", "-1%", "1%", "5%"))
names(labs) = sort(unique(sens_all_sum %>% filter(changeVar == "a12") %>% pull(changeVal)))
p <- sens_all_sum %>%
  filter(changeVar == "a12") %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  facet_grid(cols = vars(changeVal), 
             labeller = labeller(changeVal = labs)
             ) +
  guides(color = "none") +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "change in\ninvasion\ntime") +
  #lims(x = x_lims) +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(sens_all_sum %>% 
                                                  filter(changeVar =="a12") %>% 
                                                  pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines")
  )
ggsave("output/figures/sensitivity_a12_full.pdf", p, width = 12, height = 5)

#### A21 ------------------------------------------------------------
labs = paste0("effect of invader on native: ", c("-5%", "-1%", "1%", "5%"))
names(labs) = sort(unique(sens_all_sum %>% filter(changeVar == "a21") %>% pull(changeVal)))
p <- sens_all_sum %>%
  filter(changeVar == "a21") %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  facet_grid(cols = vars(changeVal), 
             labeller = labeller(changeVal = labs)
  ) +
  guides(color = "none") +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "change in\ninvasion\ntime") +
  #lims(x = x_lims) +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(sens_all_sum %>% 
                                                  filter(changeVar =="a21") %>% 
                                                  pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines")
  )
ggsave("output/figures/sensitivity_a21_full.pdf", p, width = 12, height = 5)

#### A22 ------------------------------------------------------------
labs = paste0("effect of native on native: ", c("-5%", "-1%", "1%", "5%"))
names(labs) = sort(unique(sens_all_sum %>% filter(changeVar == "a22") %>% pull(changeVal)))
p <- sens_all_sum %>%
  filter(changeVar == "a22") %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  facet_grid(cols = vars(changeVal), 
             labeller = labeller(changeVal = labs)
  ) +
  guides(color = "none") +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "change in\ninvasion\ntime") +
  #lims(x = x_lims) +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(sens_all_sum %>% 
                                                  filter(changeVar =="a22") %>% 
                                                  pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines")
  )
ggsave("output/figures/sensitivity_a22_full.pdf", p, width = 12, height = 5)


#### DispDist2 -----------------------------------------------------------------
labs = paste0("native max. dispersal dist: ", c("1","3","4"))
names(labs) = sort(unique(sens_all_sum %>% filter(changeVar == "DispDist2") %>% pull(changeVal)))
p <- sens_all_sum %>%
  filter(changeVar == "DispDist2") %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  facet_grid(cols = vars(changeVal), 
             labeller = labeller(changeVal = labs)
  ) +
  guides(color = "none") +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "change in\ninvasion\ntime") +
  #lims(x = x_lims) +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(sens_all_sum %>% 
                                                  filter(changeVar =="DispDist2") %>% 
                                                  pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    #axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines"), 
    strip.background = element_blank()
  )
ggsave("output/figures/sensitivity_dispdist_full.pdf", p, width = 10, height = 5)

