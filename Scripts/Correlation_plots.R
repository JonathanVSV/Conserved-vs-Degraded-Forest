# Packages that are needed--------------------------------
library(PerformanceAnalytics)
library(tidyverse)

# Read data-----------------------------------------------
data<-read.csv("plots_data.csv", header=T)

# Preparing data------------------------------------------
## Subset data into degraded and conserved forest
## Eliminate Plot and Forest type columns from the data to perform correlation plots

# Data for both degraded and conserved forest
both_df <- data %>%
  dplyr::select(-Plot, -Forest_type, -BA)
# Conserved forest data
conserved_df <- data %>%
  dplyr::filter(Forest_type == "conserved") %>%
  dplyr::select(-Plot, -Forest_type, -BA)
# Degraded forest data
degraded_df <- data %>%
  dplyr::filter(Forest_type == "degraded") %>%
  dplyr::select(-Plot, -Forest_type, -BA)

# Create correlation plots---------------------------------
# Both types of forest plot
chart.Correlation(both_df, 
                  method= "kendall", 
                  histogram = T, 
                  pch="+", 
                  width=30)
# Conserved forest plot
chart.Correlation(conserved_df, 
                  method= "kendall", 
                  histogram = T, 
                  pch="+", 
                  width=30)
# Degraded forest plot
chart.Correlation(degraded_df, 
                  method= "kendall", 
                  histogram = T, 
                  pch="+", 
                  width=30)

