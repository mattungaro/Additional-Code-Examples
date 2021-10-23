# Title:  Challenge: Sentiment Scoring
# File:   SentimentChallenge.R
# Course: Data Mining with R

# INSTALL AND LOAD PACKAGES ################################

# Install pacman if you don't have it (uncomment next line)
# install.packages("pacman")

# Install and/or load packages with pacman
pacman::p_load(  # Use p_load function from pacman
  gutenbergr,    # Import Project Gutenberg texts
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  rio,           # Import/export data
  tidytext,      # Text functions
  tidyverse      # So many reasons
)

# IMPORT DATA ##############################################

# Download "Little Women" by Louisa May Alcott from Project
# Gutenberg. The `gutenberg_download` function can access
# books by ID, which you can get from the Gutenberg website,
# https://www.gutenberg.org. (The book's page is
# http://www.gutenberg.org/ebooks/514.)
# df <- gutenberg_download(  # Save to `df`
#   514,                     # Book ID
#   strip = TRUE             # Remove headers and footers
# )

# Import local file
df <- import("data/LittleWomen.txt") %>% as_tibble()

# See first few lines
df

# PREPARE DATA #############################################

# Add line numbers to divide text into sections
df %<>% 
  mutate(line = row_number()) %>%
  select(-gutenberg_id) %>%        # Remove book ID number
  print()

# Tokenize the data
words <- df %>%
  unnest_tokens(word, text) %>%    # Break lines into words
  print()

# SENTIMENT FREQUENCIES ####################################

# Calculate and print score frequencies. Sentiment scores
# come from the AFINN lexicon, which scores the sentiment of
# words on a scale of -5 (most negative) to +5 (most
# positive). (Note: you may need to confirm that you want to
# download the AFINN lexicon.)
score_freq <- words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(value) %>% # Grouping by sentiment scores
  summarize(n = n()) %>%
  print()

# Graph score frequencies
score_freq %>% 
  ggplot(aes(value, n)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
  ggtitle("Little Women: Sentiment Scores by Words") +
  xlab("Sentiment Score") +
  ylab("Frequency of Words")

# SENTIMENT ARC ############################################

## Changes in sentiment over the course of the book
# Divide the text into sections of 100 lines and calculate a
# sentiment score for each section
score_arc <- words %>% 
  inner_join(get_sentiments("afinn")) %>%  # Use afinn
  group_by(section = line %/% 100) %>%     # Divide text
  summarize(score = mean(value)) %>%       # Section scores
  print()

# Plot scores by section to view narrative arc
score_arc %>% 
  ggplot(aes(section, score)) +
  geom_hline(yintercept = 0, col = "red") +
  geom_line(col = "gray") +
  geom_smooth(method = loess, col = "gray40") +  
  ggtitle("Little Women: Mean Sentiment Score by Section") +
  ylab("Mean Sentiment Score") +
  xlab("Section of 100 Lines")
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
