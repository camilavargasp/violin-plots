
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


# Basic plot ----
ggplot(survey_long_tx, 
       aes(survey_year, 
           site, 
           fill = surveyed ))+
  geom_tile()


## DF by region
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
  labs(y = "West") +
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6)+
  # scale_fill_manual(values = c("salmon", "darkseagreen"))+
  theme_bw() +
  theme(legend.position = "none", 
        # plot.title = element_text(hjust = 0.8),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_y_discrete(expand = expansion(mult = 0.02)) #expansion(mult = 0.02)

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
  labs(y = "Mainland") +
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6)+
  # scale_fill_manual(values = c("salmon", "darkseagreen"))+
  theme_bw() +
  theme(legend.position = "none", 
        # plot.title = element_text(hjust = 0.8),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_y_discrete(expand = expansion(mult = 0.02)) #expansion(mult = 0.02)


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
  labs(y = "East",
       x = "Year",
       fill = "TX") +
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.6)+
  # scale_fill_manual(values = c("salmon", "darkseagreen"))+
  theme_bw() +
  theme(legend.position = "bottom", 
        # plot.title = element_text(hjust = 0.8),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 7,
                                   angle = 45,
                                   hjust = 0.5,
                                   vjust = 0.7),
        strip.text = element_text(size = 5))+
  facet_grid(rows = vars(island),
             scales = "free_y",
             space = "free_y",
             switch = "y")+
  scale_y_discrete(expand = expansion(mult = c(0,0)))


tx_survey_plot_east


all_plots <- tx_survey_plot_west/tx_survey_plot_mainland/tx_survey_plot_east+
  plot_annotation(
    'Survey Timeseries of Selected PISCO Sites in the Santa Barbara Channel', 
    theme=theme(plot.title=element_text(hjust=0.5)))+
  plot_layout(heights = c(2, 1, 3))

all_plots

# ggsave(here("Plots//Sites_Info_Maps//tx_survey_plot.pdf"), tx_survey_plot, width = 14, height = 8)

# title = "Survey Timeseries of Selected PISCO Sites in the Santa Barbara Channel"


#NEXT STEPS TO DO: THANKS CAMILA! 
#1. Fix tile height so there is more space between site names in y-axis. 
#2. Change the format of "sites": instead of them appearing as "SCI_SOUTH_POINT_E", I want them to be only South Point East, or West, or Centre (for CEN). 
#2. Reorder the appearance of sites: On the y-axis, I would like brackets that group them in the respective regions. I want them to appear grouped by region. Top are Eastern sites, then the Western sites, then the Mainland. 

#3. Change color of cells: so that the colors of cells have 3 potential values (maybe white, light blue, and purple? for instance one color for 0 surveys, and then for other surveys). I want a diverging color palette that is color-blind friendly, cause red and green now is bad. 



#----------------- TEST ZONE -----------------------------
#2. How to reorder sites by island (indicate with a symbol, or add brackets around the sites) --> library(pBrackets) + multcompView
my_plot_with_brackets <- pBrackets(
  p = my_plot,
  categories = c("Group A", "Group B"),
  labels = c("Group 1", "Group 2"),
  type = "brace"
)

scale_x_discrete(guide = guide_prism_bracket(width = 0.12, outside = FALSE))

test + scale_y_discrete(guide = "prism_bracket")

#3. Tried changing colors to continuous gradient so we can see more variety: 0, 4, 6, 6+

# create a new variable "range" for count intervals

test_reshaped_long_tx <- survey_long_tx %>% 
  mutate(interval = case_when(
    tx == 0 ~ "0",
    tx <= 3 ~ "1-3",
    tx <= 6 ~ "4-6",
    tx <= 9 ~ "7-9",
    TRUE ~ "10-12"
  ))

#Custom colors:
custom_cols <- brewer.pal(9, "YlGnBu")[c(1, 3, 5, 6, 7)]

#Now we plot:
test <- ggplot(test_reshaped_long_tx, aes(survey_year, site, fill = interval, label = tx)) +
  geom_tile(color = "black", size = 0.05, aes(height = 1)) + #this is the line that makes tile height uneven
  geom_text(size = 2, color = "black") +
  labs(title = "Survey Timeseries of Selected PISCO Sites in the Santa Barbara Channel",
       x = "Survey Year (2000 - 2021)",
       y = "Site", 
       fill = "Surveyed") +
  scale_y_discrete(guide = guide_prism_bracket(width = 0.12, outside = FALSE)) +
  scale_fill_manual(values = custom_cols) +
  # scale_fill_brewer(palette = "BuPu") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.8)) + scale_y_discrete(expand = expansion(mult = 0.02))

test



