library(tidyverse)
library(vroom)
library(janitor)
library(widyr)
library(broom)

read_lines("inputs/AVONET_Raw_Data.csv", n_max = 5)

bird_df <- vroom("inputs/AVONET_Raw_Data.csv", delim = ",") %>% 
  clean_names() %>% 
  janitor::remove_empty(which = "cols") %>% 
  select(-c(data_type, species1_bird_life, species3_bird_tree, age, measurer, protocol, publication))

bird_df %>% 
  glimpse()

nrow(bird_df)

bird_df %>% 
  filter(if_any(where(is.numeric), is.na)) %>% 
  nrow()

bird_df %>% 
  filter(if_any(where(is.numeric), ~!is.na(.x))) %>% 
  nrow()

bird_df %>% 
  drop_na(where(is.numeric)) %>% 
  nrow()

bird_df %>% 
  count(species2_e_bird, e_bird_species_group) %>% 
  distinct(n)

# Dissimilarity matrix
bird_df <- bird_df %>% 
  drop_na(species2_e_bird, where(is.numeric)) %>% 
  group_by(species2_e_bird) %>% 
  summarize(across(where(is.numeric), mean)) %>% 
  ungroup() %>%  
  mutate(across(where(is.numeric), scale),
         id = row_number())

bird_df

bird_df %>% 
  count(species2_e_bird) %>% 
  distinct(n)

distance_df <- bird_df %>% 
  select(-id) %>% 
  column_to_rownames(var = "species2_e_bird") %>% 
  dist(method = "euclidean")

str(distance_df)

distance_df[1:5]

distance_df_tidy <- distance_df %>% 
  tidy()

distance_df_tidy %>% 
  ggplot(aes(distance)) +
  geom_histogram()

distance_df %>% 
  summary()

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(distance_df, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

hclust_groups <- cutree(hc1, k = 10) %>% 
  tidy() %>% 
  rename(species2_e_bird = names,
         group = x) %>% 
  mutate(group = as.character(group))

group_levels <- hclust_groups %>% 
  count(group) %>% 
  arrange(desc(n)) %>% 
  pull(group)

hclust_groups <- hclust_groups %>% 
  mutate(group = factor(group, levels = group_levels)) %>% 
  arrange(group)

hclust_groups %>% 
  count(group) %>% 
  arrange(group)

library(ggraph)
library(tidygraph)

as_tbl_graph(hc1)

tree <- as_tbl_graph(hc1) %>% 
  activate(nodes) %>% 
  left_join(hclust_groups, by = c("label" = "species2_e_bird"))

tree

tree_plot <- tree %>% 
  ggraph(layout = "dendrogram", circular = F) +
  geom_edge_bend() +
  geom_node_point(aes(color = group)) +
  coord_flip()

ggsave(filename = "outputs/tree_plot.png", tree_plot, width = 40, height = 40, dpi = 300) 

tree_tidy <- as_tbl_graph(distance_df_tidy) %>% 
  activate(nodes) %>% 
  left_join(hclust_groups, by = c("name" = "species2_e_bird"))

tree_tidy
