library(epade) 
library(MASS) # Check out for conflict with tidyverse::select
library(tidyverse)
library(rstatix)
library(broom)

# Read and prepare data------------------------------------

df <- read.csv("plots_data.csv", stringsAsFactors = T)

## Change data form wide to long format
df_long <- df %>%
  # Indicate column names to use
  select(Plot, Forest_type,Canopy_cover, BA, Mean_height, AGB, Mean_height, Density_branches, Density_trees) %>%
  # Prepare data: long format. Indicate which columns are going into long format
  pivot_longer(cols = c(Canopy_cover, BA, Mean_height, AGB, Mean_height, Density_branches, Density_trees),
               names_to = "Attribute",
               values_to = "value")

# Wilcoxon test---------------------------------------------

df_long %>%
  # Group by Type of forest and Attribute
  group_by(Attribute) %>%
  # Change Forest_type to numeric variable
  mutate_at(vars(Forest_type), as.numeric) %>%
  # Wilcoxon test, pipe-friendly 
  wilcox_test(value ~ Forest_type)

# Logistic regression models---------------------------------------------

## Define nesting function
## Taken from: https://github.com/tidyverse/tidyr/issues/769#issuecomment-537624093
func_nest <- function(df, vars_to_nest, new_col) {
  nest(df, !!new_col := {{ vars_to_nest }})
}

## Logistic regression models
df_long %>%
  # Remove columns that are not going to be used
  select(-Plot) %>%
  # Use func_nest to nest the data for each attribute
  func_nest(-Attribute, "data_nest") %>%
  # Get the glm fit, get the coefficients of glm (both intercept and slope) and threshold for the glm
  mutate(fit = map(data_nest, ~ glm(.$Forest_type ~ .$value, family = "binomial")),
         coef_info = map(fit, tidy),
         threshold = map(fit, function(x) dose.p(x, p = 0.5)[[1]])) %>%
  # Unnest the coefficient and threshold info
  unnest(c(coef_info, threshold))


