library(tidyverse)
library(igraph)
library(ggraph)
library(visNetwork)

## 1.1. Explore Dataset ----
# Load Dataset
nodes <- read_csv("nodes.csv")
nodes

ties <- read_csv("ties.csv")
ties

# Make network from data
g <- graph_from_data_frame(ties, directed = F, vertices = nodes)
g

# Explore nodes
V(g)

# Nodes count
vcount(g)

# Explore ties/edges
E(g)

# Ties count
ecount(g)


## 1.2. Visualizing Network ----
ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point()

## 2.1. Centrality ----
# node with the most frequent ties/edges
nodes |>
  mutate(degree = degree(g)) |>
  arrange(desc(degree))

# node with the most frequent ties and weight
nodes |>
  mutate(strength = strength(g)) |>
  arrange(desc(strength))

# node with most frequent lies in the shortest path between other nodes
nodes |>
  mutate(betweenness = betweenness(g)) |>
  arrange(desc(betweenness))

# node with the closest distance to all other nodes
nodes |>
  mutate(closeness = closeness(g)) |>
  arrange(desc(closeness))

nodes_centrality <- nodes |>
  mutate(
    degree = degree(g),
    strength = strength(g),
    betweenness = betweenness(g),
    closeness = closeness(g)
  ) 
nodes_centrality


## 2.2. Ties Betweenness ----
# ties with the most frequent shortest path through it
ties_betweenness <- ties |>
  mutate(betweenness = edge_betweenness(g, weights = 1/weight)) |>
  arrange(desc(betweenness))
ties_betweenness

ties_betweenness <- ties_betweenness |>
  left_join(nodes, by = c("from" = "id")) |>
  left_join(nodes, by = c("to" = "id")) |>
  rename("name_from" = name.x, "name_to" = name.y) |>
  arrange(desc(betweenness))
ties_betweenness

## 2.3. Visualizing Centrality ----
# weight on edges and degree on nodes
ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(aes(size = nodes_centrality$degree))

# weight on edges and strength on nodes
ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(aes(size = nodes_centrality$strength))

# betweenness on edges and degree on nodes
ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(alpha = ties_betweenness$betweenness)) +
  geom_node_point(aes(size = nodes_centrality$degree))

# filter important ties
median_betweenness <- median(ties_betweenness$betweenness)

ggraph(g, layout = "with_kk") +
  geom_edge_link(
    aes(
      alpha = ties_betweenness$betweenness, 
      filter = ties_betweenness$betweenness > median_betweenness
    )
  )

# Count weak tie
count_by_weight <- ties |>
  count(weight) |>
  mutate(percentage = 100 * n / nrow(ties))

# Visualize weak tie
is_weak <- E(g)$weight==1

ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(color = is_weak))

ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(filter = is_weak), alpha = 0.5)

## 3.1. Similarity ----
# Nodes Similarity
nodes_correlation <- as_adjacency_matrix(g, attr = "weight", names = F) |>
  as.matrix() |>
  cor()
nodes_correlation

sim <- nodes_correlation |>
  graph_from_adjacency_matrix(mode = "undirected", weighted = T) |>
  igraph::as_data_frame() |>
  left_join(nodes, by = c("from" = "id")) |>
  left_join(nodes, by = c("to" = "id")) |>
  arrange(desc(weight)) |>
  filter(from != to) |>
  as_tibble()
sim

filtered_sim <- sim |>
  filter(weight > 0.6) |>
  graph_from_data_frame(directed = F)
filtered_sim

ggraph(filtered_sim, layout = "with_kk") +
  geom_edge_link(aes(alpha = weight))
