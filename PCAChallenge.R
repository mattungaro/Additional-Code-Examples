# Title:  Challenge: PCA
# File:   PCAChallenge.R
# Course: Data Mining with R

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  datasets,      # R's built-in sample datasets
  magrittr,      # Pipes
  pacman,     
  car, 
  tidyverse      # So many reasons
)

# LOAD DATA ################################################

# Use the `swiss` dataset from R's built-in `datasets`
# package

?swiss
df <- swiss %>% as_tibble()
# EXPLORE DATA #################################################
df %>% scatterplotMatrix()


# PCA ########################################################
pc <- df %>% 
  prcomp(
    center = TRUE,
    scale = TRUE
  )

pc %>% summary()

pc %>% plot(main = "Eigenvalues") # first one accounts for a lot

pc %>% ggbiplot::ggbiplot()
## each dot is a region. 

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
