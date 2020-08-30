library(tidyverse)
library(caret)

# Read and prepare data------------------------------------

df <- read.csv("plots_data.csv", stringsAsFactors = T)

## Change data form wide to long format
df_long <- df %>%
  # Indicate column names to use
  select(Plot, Forest_type,Canopy_cover, BA, Mean_height, BA, Mean_height, Density_branches, Density_trees) %>%
  # Prepare data: long format. Indicate which columns are going into long format
  pivot_longer(cols = c(Canopy_cover, BA, Mean_height, BA, Mean_height, Density_branches, Density_trees),
               names_to = "Attribute",
               values_to = "value")

# Calculate Classification Accuracy ---------------------------------------

## Define nesting function
## Taken from: https://github.com/tidyverse/tidyr/issues/769#issuecomment-537624093
func_nest <- function(df, vars_to_nest, new_col) {
  nest(df, !!new_col := {{ vars_to_nest }})
}

df_long %>%
  # Remove columns that are not going to be used
  select(-Plot) %>%
  # Use func_nest to nest the data for each attribute
  func_nest(-Attribute, "data_nest") %>%
  # Get the glm fit, and get the fitted values
  mutate(fit = map(data_nest, ~ glm(.$Forest_type ~ .$value, family = "binomial")$fitted.values),
         fitted_vals = map(fit, function(x) as.data.frame(x)),
         real_vals = map(data_nest, function(x) as.data.frame(x$Forest_type))) %>%
  # unnest fitted and real values
  unnest(c(fitted_vals,real_vals)) %>%
  # Rename unnested columnes
  rename("fitted_vals2" = "x", "true" = "x$Forest_type") %>%
  # Transfrom binomial probs into factor levels
  mutate(fitted = ifelse(fitted_vals2 > 0.5, "degraded", "conserved")) %>%
  # Eliminate columns that we are not going to use
  select(c(-data_nest, -fit, -fitted_vals2)) %>%
  # Set columns as factors
  mutate_at(vars(fitted, true), function(x) as.factor(x)) %>%
  # Nesta data 
  func_nest(-Attribute, "data_nest_fit") %>%
  # Calculate confusion matrix accuracy
  mutate(acc = unlist(map(data_nest_fit, ~ confusionMatrix(.$true,.$fitted)$overall[1]*100)))

