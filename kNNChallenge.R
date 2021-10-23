# Title:  Challenge: kNN in R
# File:   Challenge.R
# Course: Data Mining with R

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  caret,         # Train/test functions
  e1071,         # Machine learning functions
  GGally,        # Plotting
  magrittr,      # Pipes
  mlbench,       # BreastCancer dataset
  pacman,        # Load/unload packages
  rio,           # Import/export data
  tidyverse      # So many reasons
)

# Set random seed to reproduce the results
set.seed(1)

# LOAD AND PREPARE DATA ####################################

# Use the `BreastCancer` dataset from the `mlbench` package

# Get info on dataset
?BreastCancer

# Load data
data(BreastCancer)

# Summarize raw data
summary(BreastCancer)

# Prepare data
df <- BreastCancer %>%   # Save to `df`
  select(-Id) %>%        # Remove `Id` 
  rename(y = Class) %>%  # Rename `Class` to `y`
  mutate(                # Modify several variables
    across(              # Select several variables
      -y,                # Select all except `y`
      as.numeric         # Convert selected vars to numeric
    )
  ) %>%
  na.omit() %>%          # Omit cases with missing data
  as_tibble() %>%        # Save as tibble
  print()                # Show data in Console

# Split data into training (trn) and testing (tst) sets
df %<>% mutate(ID = row_number())  # Add row ID
trn <- df %>%                      # Create trn
  slice_sample(prop = .70)         # 70% in trn
tst <- df %>%                      # Create tst
  anti_join(trn, by = "ID") %>%    # Remaining data in tst
  select(-ID)                      # Remove id from tst
trn %<>% select(-ID)               # Remove id from trn
df %<>% select(-ID)                # Remove id from df

# EXPLORE TRAINING DATA ####################################

# Bar chart of `y`
trn %>%
  ggplot() + 
  geom_bar(aes(x = y, fill = y)) 

# Randomly select a few variables and look at their plots
# in particular look at the first column and first row
trn %>% 
  select(y, 1:3)  %>%
  ggpairs(
    aes(color = trn$y),  # Color by class
    lower = list(
      combo = wrap(
        "facethist", 
        binwidth = 0.5
      )
    )
  )

# WORK ###################################################

# Define parameters for kNN
statctrl <- trainControl(
  method  = "repeatedcv",  # Repeated cross-validation
  number  = 5,             # Number of folds
  repeats = 3              # Number of sets of folds
)  

# Set up parameters to try while training (3-19)
k = rep(seq(3, 20, by = 2), 2) # using 1 is often associated with overfitting

# Apply model to training data (takes a moment)
fit <- train(
  y ~ ., 
  data = trn,                         # Use training data
  method = "knn",                     # kNN training method
  trControl = statctrl,               # Control parameters
  tuneGrid = data.frame(k),           # Search grid param.
  preProcess = c("center", "scale"),  # Preprocess
  na.action = "na.omit"
)

# Plot accuracy against various k values
fit %>% plot()                # Automatic range on Y axis
fit %>% plot(ylim = c(0, 1))  # Plot with 0-100% range

# Print the final model
fit %>% print() # model 15 is considered best and will be used as a model

# APPLY MODEL TO TEST DATA #################################

# Predict test set
pred <- predict(    # Create new variable ("predicted")
  fit,              # Apply saved model
  newdata = tst     # Use test data
)

# Get the confusion matrix
cm <- pred %>%
  confusionMatrix(reference = tst$y)

# Plot the confusion matrix
cm$table %>% 
  fourfoldplot(color = c("red", "lightblue")) # reference is the true value
# blue are accurate - true positive

# Print the confusion matrix
cm %>% print()

# RESULTS ##################################################

# kNN Model
# Accuracy : 0.9659          
# 95% CI : (0.9309, 0.9862)
# No Information Rate : 0.6683          
# P-Value [Acc > NIR] : <2e-16  


# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from the environment

# Clear packages
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
