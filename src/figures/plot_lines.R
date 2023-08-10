#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# load data
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

results_noN2 = read.csv("output/simulations/all_sims_noN2.csv") %>%
  select(-X)
rsum_noN2 = results_noN2 %>%
  group_by(id, DistColStart, DistColEnd, Dist1, Dist2) %>%
  summarise(mean_spread_time = mean(invasion_time))
no_control_noN2 = rsum_noN2 %>%
  filter(Dist1 == 0)
rsum_noN2 = rsum_noN2 %>%
  filter(Dist1 != 0) %>%
  mutate(delta_spread_time = mean_spread_time - no_control_noN2$mean_spread_time)



#### FIG 2: CONSISTENT EFFORT --------------------------------------------------
# create data.frame with plotting data
plot_all <- bind_rows(list(rsum %>% mutate(type = "both_sp"), 
                           rsum_noN2 %>% mutate(type = "inv_only"))) %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort %in% c(3,7,13,17,23), DistColEnd < 16)

# create data.frame to add color scale on plot
grad <- data.frame(x = min(plot_all$DistColStart), 
                   y = seq(min(plot_all$delta_spread_time)-1, 
                           max(plot_all$delta_spread_time)+1, 
                           length.out = 100), 
                   effort = 3)

# create plot
plot_all %>%
  ggplot(aes(x = DistColStart, y = delta_spread_time)) + 
  #geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(linetype = type), size = 0.6) +
  geom_line(data = grad, 
            aes(x=x, y=y, color=y),size=8)+
  facet_grid(cols = vars(effort), 
             labeller = label_both) + 
  guides(color = "none") +
  labs(x = "starting location of management\n(relative to invader wave front)", 
       y = "change in invasion time (years)") +
  scale_color_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(rsum %>% filter(DistColEnd < 16) %>% pull(delta_spread_time)))) +
  scale_linetype_manual(labels = c("model with native and invader", "model with invader only"), 
                        values = c("solid", "longdash")) +
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
        strip.background = element_blank()) #+
  #panel_border(color = "black")
ggsave("output/figures/lines_alleffort.pdf", width = 5.5, height = 2, scale = 1.5)


plot_all %>%
  filter(effort == 7) %>%
  ggplot(aes(x = DistColStart, y = delta_spread_time)) + 
  #geom_hline(yintercept = 0, linetype = "dashed")+
  geom_line(aes(linetype = type), size = 0.6) +
  geom_line(data = grad %>% filter(abs(y)< 10), 
            aes(x=x, y=y, color=y),size=8)+
  guides(color = "none") +
  labs(x = "starting location of management\n(relative to invader wave front)", 
       y = "change in invasion time") +
  scale_color_distiller(direction = 0, 
                        palette = "PuOr", 
                        limits = c(-1,1)*max(abs(rsum %>% filter(DistColEnd < 16) %>% pull(delta_spread_time)))) +
  scale_linetype_manual(labels = c("model with native and invader", "model with invader only"), 
                        values = c("solid", "longdash")) +
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
        strip.background = element_blank()) #+
#panel_border(color = "black")
ggsave("output/figures/lines.pdf", width = 6, height = 6)

