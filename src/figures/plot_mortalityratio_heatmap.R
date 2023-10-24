#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)

set.seed(76)

source("src/model/simulation_model.R")
source("src/model/setup_model_simulations.R")

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

wave_actual = calc_wave_front(IC$N1, 1)[1]
IC_shift <- data.frame(location = rep(1:ncol(IC$N1),2) - wave_actual,
                       abund = c(IC$N1[1,], IC$N2[1,]), 
                       species = c(rep("N1", ncol(IC$N1)),rep("N2", ncol(IC$N1)))) %>%
  reshape2::dcast(location ~ species, value.var = "abund")

#### CALCULATE PESTICIDE-INDUCED MORTALITY -------------------------------------
rsum$N1_IC_abund <- NA
rsum$N2_IC_abund <- NA
for(i in 1:nrow(rsum)){
  tmp_IC_shift <- IC_shift %>% 
    filter(location >= unlist(rsum[i, "DistColStart"]), 
           location <= unlist(rsum[i, "DistColEnd"]))
  rsum[i, "N1_IC_abund"] <- sum(tmp_IC_shift$N1)
  rsum[i, "N2_IC_abund"] <- sum(tmp_IC_shift$N2)
}


#### HEATMAP WITH MORTALITY RATIO -----------------------------------------------
detection_thresh = 100
detection_location = calc_wave_front(IC$N1, detection_thresh)[1]
wave_actual = calc_wave_front(IC$N1, 1)[1]
x_lims = c(-31, 31)

# create data.frame to add color scale on plot
grad <- data.frame(x = log(min(rsum$N1_IC_abund/rsum$N2_IC_abund)), 
                   y = seq(min(rsum$delta_spread_time)-1, 
                           max(rsum$delta_spread_time)+1, 
                           length.out = 100))

rect = rsum %>%
  group_by(DistColStart) %>%
  filter(DistColEnd == max(DistColEnd)) %>%
  mutate(
    xmin = DistColStart - 1,
    xmax = DistColEnd + 1, 
    ymin = DistColStart - 0.45,
    ymax = DistColStart + 0.45) %>%
  select(xmin, xmax, ymin, ymax)


rsum %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = log(N1_IC_abund/N2_IC_abund)), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  geom_text(data = data.frame(x = rsum$DistColStart %>% unique()-1.5, 
                              y = -1*rsum$DistColStart %>% unique(), 
                              lab = rsum$DistColStart %>% unique()), 
            aes(x = x, y = y, label = lab), hjust = 1, size = 2) +
  geom_text(data = data.frame(x = rsum$DistColEnd %>% unique(), 
                              y = 29, 
                              lab = rsum$DistColEnd %>% unique()), 
            aes(x = x, y = y, label = lab), vjust = 0, size = 2) +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "mortality ratio") +
  #lims(x = x_lims) +
  guides(color = "none") +
  scale_fill_distiller(direction = 0,
                       palette = "PiYG",
                       limits = c(-1,1)*max(abs(rsum %>% 
                                                  mutate(f = log(N1_IC_abund/N2_IC_abund)) %>% 
                                                  filter(!is.infinite(f)) %>%
                                                  pull(f)))) +
  scale_x_continuous(expand = c(0,0), 
                     limits = x_lims) +
  theme_classic() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(),
    axis.title.y = element_blank(),#element_text(angle = 0, vjust =0.95),
    axis.ticks = element_blank(),
    legend.position = c(0.9, 0.5),
    panel.grid = element_blank(), 
    panel.spacing = unit(0.1, "lines"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
ggsave("output/figures/heatmap_mortality_rate.pdf", width = 6, height = 6)

