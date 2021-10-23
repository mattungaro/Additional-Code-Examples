# Title:  Challenge: Decomposition
# File:   DecompositionChallenge.R
# Course: Data Mining with R

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  changepoint,   # Changepoint analysis
  datasets,      # R's built-in sample datasets
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  rio,           # Import/export data
  tidyverse      # So many reasons
)

# LOAD AND PREPARE DATA ####################################

# Use `EuStockMarkets` data from R's `datasets` package.
# This data gives the daily closing prices of major European
# stock indices: Germany's DAX (Ibis), Switzerland's SMI,
# France's CAC, and the UK's FTSE.

# Get info on the dataset
?EuStockMarkets

# See the data (with decimal dates)
EuStockMarkets

# Plot the data
EuStockMarkets %>% plot()


df <- EuStockMarkets[, 3]  # This selects the third series

# Plot the CAC data
df %>%
    plot(
    main = "Stock Markets",
    xlab = "Year",
    ylab = "Closing Price",
    ylim = c(0, 5000)
  )

# DECOMPOSE TIME SERIES ####################################

# Uses the `decompose` function from R's built-in `stats`
# package; default method is "additive"
df %>% 
  decompose() %>%
  plot()

## decomposition removes seasonal element
## linear trend plus seasonal, you end up with the leftovers - unexplained, random component
## great for looking at the data
## 

# Can also specify a multiplicative trend (when things are increasing by a percentage), 
# which is good for
# trends that spread over time; the scales for the seasonal
# and random components are now multipliers instead of
# addends.

##
df %>% 
  decompose(
    type = "multiplicative"
  ) %>%
  plot()


# CHANGEPOINTS ############################################

# Compute and plot time series with change points; can look
# for changepoints in mean using `cpt.mean()`, in variance
# with `cpt.var()`, or both with `cpt.meanvar()`.
df %>%
  cpt.mean(
    test.stat = "Normal"
  ) %T>%                  # T-pipe
  plot(                   # Add change point lines to plot
    cpt.width = 3,        # Line width
    main = "Change Points for Stock Market",
    xlab = "Year"
  ) %>% 
  cpts.ts()               # Print change point location(s)

## Looks at changes in the mean. Change point shows when the average has to change.
## kind of like breakpoint analysis?
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
