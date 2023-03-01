#### PREAMBLE ------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cowplot)

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


#### RESULTS -------------------------------------------------------------------
# neutral example
rsum %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort == 13, 
         DistColEnd %in% seq(-6,-2,2))

# decrease invasion time example
rsum %>% 
  filter(DistColStart == -12, 
         DistColEnd == 0)

rsum %>% 
  filter(DistColStart == -22)


# range of strategies that decrease invasion time
rsum %>% 
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(delta_spread_time < -1) %>%
  ungroup() %>%
  summarise(mn = min(effort), 
            mx = max(effort))


# invader only
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

rsum_noN2 %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort == 7)

rsum %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort == 7)

# misidentify wave front
rsum %>%
  mutate(effort = DistColEnd - DistColStart + 1) %>%
  filter(effort == 13)


# most effective strategy
rsum %>%
  ungroup() %>%
  filter(delta_spread_time == max(delta_spread_time))

# most effective strategy (at detection threshold)
rsum %>% 
  filter(DistColStart == -28, 
         DistColEnd == -8)
