## Example from data-to-vis

# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/zonination/perceptions/master/probly.csv", header=TRUE, sep=",")

colnames(data)
head(data)

## data cleaning
data_clean <- data %>% 
  gather(key="text", value="value") %>%
  mutate(text = gsub("\\.", " ",text)) %>%
  mutate(value = round(as.numeric(value),0)) %>%
  filter(text %in% c("Almost Certainly","Very Good Chance","We Believe","Likely","About Even", "Little Chance", "Chances Are Slight", "Almost No Chance")) %>% 
  mutate(text = fct_reorder(text, value))

colnames(data_clean)

# Plot
data_clean %>%
  ggplot( aes(x=text, 
              y=value, 
              fill=text, 
              color=text)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() +
  xlab("") +
  ylab("Assigned Probability (%)")
