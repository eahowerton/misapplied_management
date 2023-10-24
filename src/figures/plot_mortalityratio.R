#### PREAMBLE
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)

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

#### FIG 3: PLOT MORTALITY RATIO VS. CHANGE IN INV. TIME -----------------------
grad <- data.frame(x = log(min(rsum$N1_IC_abund/rsum$N2_IC_abund)), 
                   y = seq(min(rsum$delta_spread_time)-1, 
                           max(rsum$delta_spread_time)+1, 
                           length.out = 100))

p <- ggplot(data = rsum %>% 
         mutate(d_spread_time_cat = ifelse(delta_spread_time > 1, 1,
                                           ifelse(delta_spread_time < -1, -1, 0)), 
                letter_labs = ifelse(paste(DistColStart, DistColEnd, sep = "_") == 
                                       paste(achieve_dist_locs[1], achieve_dist_locs[2], sep = "_"), "A", 
                                     ifelse(paste(DistColStart, DistColEnd, sep = "_") == 
                                              paste(waste_dist_locs[1], waste_dist_locs[2], sep = "_"), "B", 
                                            ifelse(paste(DistColStart, DistColEnd, sep = "_") == 
                                                     paste(worse_dist_locs[1], worse_dist_locs[2], sep = "_"), "C", NA)))),
         aes(x = log(N1_IC_abund/N2_IC_abund), y = delta_spread_time)) +#, fill = delta_spread_time)) + 
  annotate("rect", xmin = -Inf, xmax = -10, ymin = -1, ymax = 1, fill = "grey") +
  annotate("rect", xmin = -Inf, xmax = -10, ymin = -Inf, ymax = -1, fill = brewer.pal(6, "PuOr")[2]) +
  annotate("rect", xmin = -Inf, xmax = -10, ymin = 1, ymax = Inf, fill = brewer.pal(6, "PuOr")[5]) +
  geom_vline(aes(xintercept = 0), size = 0.25) +
  geom_hline(aes(yintercept = -1), size = 0.25) +
  geom_hline(aes(yintercept = 1), size = 0.25) +
  # geom_line(data = grad,
  #           aes(x=x, y=y, color=y),size=8)+
  geom_line(aes(group = DistColEnd - DistColStart + 1), color = "grey", size = 0.2, linetype = "dashed") + 
  geom_point(aes(fill = as.factor(d_spread_time_cat)), shape = 21, size = 1.4, stroke = 0.25) + # shape = 21, color = 'black', fill = "white" #
  geom_text(data = data.frame(x = c(-1, 1)*0.75, 
                              y = Inf, 
                              lab = c("more natives killed", "more invaders killed"), 
                              hjust = c(1,0)), 
            aes(x = x, y = y, label = lab, hjust = hjust), vjust = 1, size = 2) +
  #facet_wrap(vars(DistColEnd - DistColStart + 1)) +
  scale_color_distiller(direction = 0, 
                        palette = "PuOr", 
                        limits = range(grad$y)) + 
  scale_fill_manual(values = c(brewer.pal(6, "PuOr")[2],"grey65",brewer.pal(6, "PuOr")[5])) +
  scale_x_continuous(name = "mortality ratio = log(invaders killed/natives killed)") +
  scale_y_continuous(breaks = c(-20,-10,-1,1,10,20), 
                     limits = c(-1,1)*max(abs(rsum$delta_spread_time)),
                     name = "change in invasion time (years)") +
  theme_bw(base_size = 8) + 
  theme(
    legend.position = "none", 
        panel.grid= element_blank())
ggsave("output/figures/mortality_rate.pdf", p, width = 7.75, height = 7.75, units = "cm", scale = 1)

