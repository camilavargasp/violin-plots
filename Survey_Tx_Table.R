
## Load libraries ----
library(tidyverse)
library(viridis)
library(patchwork)

# 1. LOAD DATA ----
upc_swath_ucsb <- read_csv("data/upc_swath_ucsb_ALL.csv")

## Create DF with site names and region - used to create table for Sofia to fill in
# site_names <- upc_swath_ucsb %>% 
#   select(site, region) %>% 
#   distinct()

## Read in site name key
sites_data <- read_csv("data/upc_sites_sofia.csv") %>% 
  mutate(site_clean = str_to_title(site_clean))


# 2. SITE SURVEY TIMESERIES (TABLE) ----
#Table of sites & timeseries data. Before creating a figure, prep the data. 

# Dplyr method ----
# Reshape the data using pivot_wider
survey_tx <- upc_swath_ucsb %>%
  pivot_wider(id_cols = site, 
              names_from = survey_year,
              values_from = n.swath.tx) %>%
  #then we repopulate NAs with zeroes
  mutate_all(~replace(., is.na(.), 0))

#To plot more easily we reshape data from wide to long
survey_long_tx <- survey_tx %>%
  pivot_longer(
    cols = -site,
    names_to = "survey_year",
    values_to = "tx"#pivot all columns except site
  ) %>% # we add a column for surveyed: YES/NO
  mutate(surveyed = ifelse(tx == 0, "No", "Yes")) %>% 
  left_join(sites_data, by = "site")



## DF by region ----
east_sites <- survey_long_tx %>% 
  filter(region == "East")

mainland_sites <- survey_long_tx %>% 
  filter(region == "Mainland")

west_sites <- survey_long_tx %>% 
  filter(region == "West")


# Polished plot by region ----
## WEST----
tx_survey_plot_west <- ggplot(west_sites, 
                         aes(survey_year, 
                             site_clean, 
                             fill = tx, 
                             label = tx)) +
  geom_tile(color = "black", 
            linewidth = 0.05, 
            aes(height = 1)) + #this is the line that makes tile height uneven
  geom_text(size = 2, 
            color = "black") +
  # labs(y = "West") +
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6,
                       limits = c(0, 12),
                       breaks = c(0, 4, 8, 12))+
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))

tx_survey_plot_west  

## MAINFLAND ----
tx_survey_plot_mainland <- ggplot(mainland_sites, 
                                  aes(survey_year, 
                                      site_clean, 
                                      fill = tx, 
                                      label = tx)) +
  geom_tile(color = "black", 
            linewidth = 0.05, 
            aes(height = 1)) + #this is the line that makes tile height uneven
  geom_text(size = 2, 
            color = "black") +
  # labs(y = "Mainland") +
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6,
                       limits = c(0, 12),
                       breaks = c(0, 4, 8, 12))+
  # scale_fill_manual(values = c("salmon", "darkseagreen"))+
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))


tx_survey_plot_mainland

## EAST ----
tx_survey_plot_east <- ggplot(east_sites, 
                              aes(survey_year, 
                                  site_clean, 
                                  fill = tx, 
                                  label = tx)) +
  geom_tile(color = "black", 
            linewidth = 0.05, 
            aes(height = 1)) + #this is the line that makes tile height uneven
  geom_text(size = 2, 
            color = "black") +
  labs(
       x = "Year") + #y = "East",
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6,
                       limits = c(0, 12),
                       breaks = c(0, 4, 8, 12))+
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7,
                                   angle = 45,
                                   hjust = 0.5,
                                   vjust = 0.7),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))


tx_survey_plot_east

## Combining plot using the patchwork package ----

all_plots <- tx_survey_plot_west/tx_survey_plot_mainland/tx_survey_plot_east+
  plot_annotation(
    'Survey Timeseries of Selected PISCO Sites in the Santa Barbara Channel', 
    theme=theme(plot.title=element_text(hjust=0.5)))+
  plot_layout(heights = c(2, 1, 3),
              guides = "collect") &
  theme(legend.position = "bottom")

all_plots

## Save plot to png ----
ggsave("plots/tx_survey_plot.png", all_plots, width = 8, height = 10)

