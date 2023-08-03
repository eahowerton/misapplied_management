#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

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

#IC <- read.csv()

#### FIG 1A: INITIAL ABUNDANCES ------------------------------------------------
detection_thresh = 100
detection_location = calc_wave_front(IC$N1, detection_thresh)[1]
wave_actual = calc_wave_front(IC$N1, 1)[1]
x_lims = c(-31, 31)

species_labels = data.frame(x = c(x_lims[1] + 2,x_lims[2]-2), 
                            y = c(max(IC$N1[1,]) - 700, max(IC$N2[1,])+700), 
                            text = c("invader", "native"), 
                            hjust = c(0,1))

location_breaks = c(seq(-28,detection_location-wave_actual,14), 
                   detection_location-wave_actual, 
                   seq(0,28,14))
location_labels = as.character(location_breaks)
location_labels[location_labels == as.character(detection_location-wave_actual)] = 
  paste0(detection_location-wave_actual,"\ndetectable\nfront")
location_labels[location_labels == "0"] = "0\nwave\nfront"

# plot results
p1 = data.frame(location = rep(1:ncol(IC$N1),2) - wave_actual,
                abund = c(IC$N1[1,], IC$N2[1,]), 
                species = c(rep("N1", ncol(IC$N1)),rep("N2", ncol(IC$N1)))) %>%
  ggplot(aes(x = location, y = abund)) + 
  geom_line(aes(color = species), size = 1) + 
  geom_text(data = species_labels, aes(x = x, y = y, label = text, hjust = hjust), size = 3) +
  #geom_hline(yintercept = detection_thresh, linetype = "longdash") +
  labs(x = "location on landscape", y = "density") +
  scale_color_manual(values = c("grey75", "black")) +
  scale_x_continuous(breaks = location_breaks,#c(detection_location-wave_actual,wave_actual-wave_actual),
                     expand = c(0,0), 
                     labels = location_labels,
                     #labels = c("detectable\nfront","wave\nfront"),
                     limits = x_lims)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 5.5),
        axis.text.y =element_blank(), # axis.text.y
        axis.ticks.y = element_blank(), #  axis.ticks.y
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust =0.95),
        legend.position = "none",
        plot.title = element_text(size = 12))
p1

#### FIG 1B: CHANGE IN INVASION TIME -------------------------------------------
rect = rsum %>%
  group_by(DistColStart) %>%
  filter(DistColEnd == max(DistColEnd)) %>%
  mutate(
    xmin = DistColStart - 1,
    xmax = DistColEnd + 1, 
    ymin = DistColStart - 0.45,
    ymax = DistColStart + 0.45) %>%
  select(xmin, xmax, ymin, ymax)

pnts = rsum %>%
  group_by(DistColStart) %>%
  filter(delta_spread_time == max(delta_spread_time)) %>%
  mutate(colr = ifelse(delta_spread_time < -1, "dec", 
                       ifelse(delta_spread_time > 1, "inc", "no change")))

letter_labs = data.frame(y = -1*c(achieve_dist_locs[1], waste_dist_locs[1], worse_dist_locs[1]),
                         x = c(achieve_dist_locs[2], waste_dist_locs[2], worse_dist_locs[2]), 
                         lab = LETTERS[1:3])

p2 = rsum %>%
  ggplot() + 
  geom_tile(aes(x = DistColEnd,
                y = -DistColStart, 
                fill = delta_spread_time), height = 0.9, alpha = 0.8) +
  geom_rect(data = rect,
            aes(xmin = xmin,xmax = xmax,
                ymin = -ymin, ymax = -ymax), 
            fill = NA, color = "grey45") +
  geom_point(data = pnts,
             aes(x = DistColEnd, y = -DistColStart, color = colr), size = 1.5) +
  geom_text(data = data.frame(x = rsum$DistColStart %>% unique()-1.5, 
                              y = -1*rsum$DistColStart %>% unique(), 
                              lab = rsum$DistColStart %>% unique()), 
            aes(x = x, y = y, label = lab), hjust = 1, size = 2) +
  geom_text(data = data.frame(x = rsum$DistColEnd %>% unique(), 
                              y = 29, 
                              lab = rsum$DistColEnd %>% unique()), 
            aes(x = x, y = y, label = lab), vjust = 0, size = 2) +
  geom_text(data = letter_labs, aes(x = x, y = y, label = lab), size = 2) + 
  # geom_line(data =  rsum  %>%
  #             mutate(effort = DistColEnd - DistColStart + 1) %>%
  #             filter(effort == 13),
  #           aes(x = DistColEnd, y = -DistColStart), 
  #           linetype = "dotted") +
  labs(x = "ending location of management", 
       y = "starting location\nof management", 
       fill = "change in\ninvasion time\n(years)") +
  #lims(x = x_lims) +
  guides(color = "none") +
  scale_fill_distiller(direction = 0, 
                       palette = "PuOr", 
                       limits = c(-1,1)*max(abs(rsum %>% pull(delta_spread_time)))) +
  scale_color_manual(values = c(brewer.pal(6, "PuOr")[6],"grey65")) +
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


p = plot_grid(p1, p2, 
              axis = "l",align = "v",
              ncol = 1, rel_heights = c(1/3, 2/3))
p

ggsave("output/figures/figure1.pdf", width = 6, height = 6)

