###################
##  Sample Code  ##
###################

####  Startup  ####
## Load packages
library(tidyverse)
library(assertr)
library(here)



####  Load Data  ####
# The here() package returns filepaths based on the arguments you give it,
# starting from the folder with your .Rproj file; in this case, it will return
# the filepath to the dataset inside the "data" folder
# This is especially useful when a script will be run on Windows and Mac 
# computers, as they use different folder separators ("/" vs "\")
df <- readRDS(here("data", "T2T Data for Pinder et al.rds"))



####  Prepare Data  ####
df <- df %>%
  
  # Rename variables
  rename(id = yb_lsmh_id) %>%
  
  # Create new variables
  mutate(race_recoded = "...") %>%
  
  # Confirm no missing values
  verify(!is.na(yb_cdi_1)) %>% #see vignette("assertr")
  
  # Select only variables needed for analysis
  select(everything())



####  Analysis  ####
## Correlations
cor.test(df$yb_bads_1, df$yb_bads_10, method = "spearman")


## Plots
df %>%
  ggplot() +
  geom_histogram(aes(yb_bads_1))
