## Wrod cloud code

## Load packages
library(tidyverse)
library(lubridate)
library(tidytext)
library(wordcloud)
library(janitor)
library(here)
# library(SnowballC)

## Data
equity_df <- tibble(equity = c("To provide resources and support to individuals based on their needs and circumstances
Everyone has what they need to succeed
Everyone is seen
Recognizing that everyone has different needs, and making an effort to meet those needs
Acknowledging that different people face different challenges while approaching the same task, so different resources and opportunities may need to be provided so everyone can succeed.
Recognizing the diversity of identities and experiences that have shaped us and accounting for them."))



social_justice_df <- tibble(social_justice = c("It is a harder concept to define.
 I always pair it with action in my brain? Like what am I doing in the classroom to make the world better for communities that are marginalized by the systems in place
ensuring that things are done right for people: this may include reparations, opportunities, change in paradigm and approaches
dismantling systems of oppression and focusing on reparations
Recognizing that different social groups have been discriminated and excluded, resulting in chronic disadvantages and lack of resources. Then actively working towards correcting, repairing and preventing these unjust circumstances"))

data("stop_words")

stop_word_vec <- stop_words$word


## Tokens

equity_token <- equity_df %>% 
  unnest_tokens(word, equity) %>% 
  anti_join(stop_words) %>% 
  count(word)
  

social_justice_token <- social_justice_df %>% 
  unnest_tokens(word, social_justice) %>% 
  anti_join(stop_words) %>% 
  count(word)


## plot

library(ggwordcloud)

equity_token %>% 
  # top_n(50) %>% 
  ggplot(aes(label = word, 
             size = n)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()

social_justice_token %>% 
  filter(word != "brain") %>% 
  # top_n(50) %>% 
  ggplot(aes(label = word, 
             size = n)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()
