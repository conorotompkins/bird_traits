library(tidyverse)
library(vroom)
library(janitor)

read_lines("inputs/AVONET_Raw_Data.csv", n_max = 5)

bird_df <- vroom("inputs/AVONET_Raw_Data.csv", delim = ",") %>% 
  clean_names()

bird_df %>% 
  glimpse()

bird_df_clean <- bird_df %>% 
  janitor::remove_empty(which = "cols") %>% 
  select(-c(species1_bird_life, species3_bird_tree, measurer, protocol, publication))

bird_df_clean %>% 
  glimpse()

nrow(bird_df_clean)

bird_df_clean %>% 
  filter(if_any(where(is.numeric), is.na)) %>% 
  nrow()

bird_df_clean %>% 
  filter(if_any(where(is.numeric), ~!is.na(.x))) %>% 
  nrow()

bird_df_clean %>% 
  drop_na(is.numeric) %>% 
  nrow()

bird_df_clean %>% 
  count(species2_e_bird, e_bird_species_group) %>% 
  distinct(n)


# Dissimilarity matrix
distance_df <- bird_df_clean %>% 
  drop_na(is.numeric) %>% 
  group_by(species2_e_bird) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  summarize(across(where(is.numeric), scale)) %>% 
  dist(method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(distance_df, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
