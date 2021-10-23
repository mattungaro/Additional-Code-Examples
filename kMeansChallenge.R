# Title:  Challenge: k-Means
# File:   kMeansChallenge.R
# Course: Data Mining with R

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  datasets,      # R's built-in sample datasets
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  rio,           # Import/export data
  tidyverse,
  factoextra,
cluster
)

# LOAD AND PREPARE DATA ####################################

# Use the `iris` dataset from R's `datasets` package

?iris               # Get info on dataset
iris %>% head()     # See first few lines
iris %>% summary()  # Get summary statistics

# Use k = 3 for k-means clustering


# Separate the class labels
species <- iris %>%  # Rename `y` back to `species`
  pull(Species)          # Select just `y` as a vector

iris %<>% 
  select(-Species) %>%   # Select everything except `y`
  scale()          # Standardize variables

# OPTIMAL NUMBER OF CLUSTERS ###############################

# Elbow method
iris %>%
  fviz_nbclust(          # From `factoextra`
    FUN = kmeans,        # Use k-means
    method = "wss"       # "within cluster sums of squares"
  )                    # Look for "bend" in curve

# Silhouette method
iris %>%
  fviz_nbclust(
    FUN = kmeans,          # Use k-means
    method = "silhouette"  # Look for maximum width
  ) 

# Use gap statistics to find optimal number of clusters
# and visualize it using fviz_gap_stat
iris %>% 
  clusGap(         # Function from `cluster`
    FUN = kmeans,  # Method for clustering
    K.max = 10,    # Maximum number of clusters
    B = 100        # Number of Monte Carlo/bootstrap samples
  ) %>%
  fviz_gap_stat()  # Look for highest value

# K-MEANS CLUSTERING #######################################

# Compute three clusters
km <- iris %>% 
  kmeans(3) %>%  # Set the number of clusters
  print()        # Print output

# Visualize the clusters
km %>% fviz_cluster(
  data = iris,
  geom = c("point")
) +
  geom_text(
    vjust = 1.5,  # Color points according to cluster
    aes(
      color = as.factor(km$cluster),
      label = species # label according to species
    )
  )

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
detach("package:datasets", unload = T)  # For base packages
p_unload(all)    # Remove all contributed packages

# Clear plots
graphics.off()   # Clears plots, closes all graphics devices

# Clear console
cat("\014")      # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)
