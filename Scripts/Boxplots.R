library(tidyverse)
library(cowplot)
library(plotrix)

# Read data -------------------
df <- read.csv("plots_data.csv")

# Transform data into long format and calculate mean and se by Forest_type and Attribute
df <- df %>%
  # Select columns that are going to be used
  select(Plot, Forest_type,Canopy_cover, BA, Mean_height, AGB) %>%
  # Change data into long format
  pivot_longer(cols = c(Canopy_cover, BA, Mean_height, AGB),
               names_to = "Attribute",
               values_to = "value") 

# Build boxplots --------------------

## First plot
p1 <- df  %>%
  # Filter data just to stay with basal area data
  filter(Attribute == "BA") %>%
  # Do ggplot
  ggplot(aes(x = Forest_type, 
             y = value)) + 
  # Add boxplot
  geom_boxplot(fill = "gray90", width = 0.6) +
  # Add threshold line
  geom_hline(yintercept = 9.45, col = "red", lty = "longdash")+
  # Add text for the threshold's value
  geom_text(aes(x = 1.5, y = 10.3, label = "9.45")) +
  # Change the x and y axis titles
  labs(x = "Forest type", y = expression(bold("Basal"~"area"~"("*"m"^2*"/"*"ha"*")"))) +
  # Change x axis levels
  scale_x_discrete(labels = c("Conserved\nforest","Degraded\nforest")) +
  # Change y axis breaks and limits 
  scale_y_continuous(breaks = seq(0,25,5),
                     limits = c(0,25),
                     expand = c(0,0)) +
  # Add cowplot theme
  theme_cowplot() + 
  # Remove x axis title and set the axis titles as bold face
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_blank())
p1

# All the following plots use the same approach as p1, but use different data

## Second plot
p2 <- df  %>%
  filter(Attribute == "AGB") %>%
  ggplot(aes(x = Forest_type, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) +
  geom_hline(yintercept = 27.5, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 29.5, label = "27.5"))+
  labs(x = "Forest type", y = "Biomass (Mg/ha)") +
  scale_x_discrete(labels = c("Conserved\nforest","Degraded\nforest")) +
  scale_y_continuous(breaks = seq(0,70,10),
                     limits = c(0,70),
                     expand = c(0,0)) +
  theme_cowplot()+ 
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_blank())
p2

## Third plot
p3 <- df  %>%
  filter(Attribute == "Canopy_cover") %>%
  ggplot(aes(x = Forest_type, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) + 
  geom_hline(yintercept = 90.9, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 94.5, label = "90.9"))+
  labs(x = "Forest type", y = "Canopy cover (%)") +
  scale_x_discrete(labels = c("Conserved\nforest","Degraded\nforest")) +
  scale_y_continuous(breaks = seq(0,100,25),
                     limits = c(0,100),
                     expand = c(0,0)) +
  theme_cowplot()+ 
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_blank())
p3

## Fourth plot
p4 <- df  %>%
  filter(Attribute == "Mean_height") %>%
  ggplot(aes(x = Forest_type, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) + 
  geom_hline(yintercept = 5.30, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 5.60, label = "5.30"))+
  labs(x = "Forest type", y = "Mean height (m)") +
  scale_x_discrete(labels = c("Conserved\nforest","Degraded\nforest")) +
  scale_y_continuous(breaks = seq(0,8,2),
                     limits = c(0,8),
                     expand = c(0,0)) +
  theme_cowplot()+ 
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_blank())
p4

## Join 4 plots in grid with horizontal and vertical align
p_final <- plot_grid(p1, 
                     p2, 
                     p3,
                     p4, 
                     labels = paste0("(",letters[1:4],")"),
                     align = "hv",
                     label_size = 18,
                     label_x = 0, 
                     label_y = 0,
                     hjust = -0.5, 
                     vjust = -2)
p_final

## Save joined plot
save_plot("Final_boxplot_beta.jpg", p_final, 
          base_height = 8,
          base_asp = 0.85)

