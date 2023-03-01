#### PREAMBLE ------------------------------------------------------------------
library(usmap)
library(ggplot2)
library(RColorBrewer)
library(tidycensus)

inv_year = read.csv("RIFAByCounty.csv")

states_to_plot = inv_year$State.Abbreviation %>% unique()

# load FIPS key 
rm(fips_codes)
fips_codes <- force(fips_codes)
fips_codes <- fips_codes %>% 
  filter(state %in% states_to_plot) %>%
  mutate(county = gsub("\\s*\\w*$", "", county))


# join to inv_year for plotting
plt = inv_year %>%
  #mutate(County = paste(County, "County")) %>%
  #select(-County.Upper) %>%
  rename(state_name = State, 
         state = State.Abbreviation, 
         county = County) %>%
  right_join(fips_codes) %>%
  mutate(fips = paste0(state_code, county_code))



#### FIGURE 1 ------------------------------------------------------------------
counties <- plot_usmap(include = states_to_plot, 
           regions = "counties", 
           data = plt, 
           values = "Year.Invaded", 
           color = "white") + 
  scale_fill_distiller(palette = "YlOrBr", 
                       na.value = "grey") +
  theme(legend.position = "bottom")
  
states <- plot_usmap(include = states_to_plot, 
                     regions = "states",
                     color = "black")
  
Mobile <- data.frame(lon = -88.0399, lat = 30.6954) #data.frame(lon = -88.0399, lat = 30.6954)
Mobile <- usmap_transform(Mobile)


ggplot() +  
  geom_polygon(data=counties[[1]], 
               aes(x=x, 
                   y=y, 
                   group=group, 
                   fill = counties[[1]]$Year.Invaded), 
               color = "white",
               size = 0.1) +  
  geom_polygon(data=states[[1]], 
               aes(x=x, 
                   y=y, 
                   group=group), 
               color = "black", 
               fill = alpha(0.001), 
               size = 0.25) + 
  geom_point(data = Mobile, aes(x = x, y = y)) +
  coord_equal() +
  scale_fill_distiller(palette = "YlOrRd", 
                       na.value = "grey",
                       name = "quarantine\nyear") +
  theme_map() +
  theme(legend.position = "bottom", 
        legend.key.width = unit(1.25, "cm"))
ggsave("output/figures/map.pdf", width = 5.5, height = 3.5, scale = 1.5)
  
  