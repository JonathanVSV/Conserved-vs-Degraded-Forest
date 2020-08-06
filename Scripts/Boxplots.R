library(tidyverse)
library(cowplot)
library(plotrix)

# Read data -------------------
df <- read.csv("plots_data.csv")

# Transform data into long format and calculate mean and se by tipo_bosque and Attribute
df <- df %>%
  # Select columns that are going to be used
  select(Sitio, tipo_bosque,Cobertura, AB_m2.ha, Media_altura, biomasa_mg.ha) %>%
  # Change data into long format
  pivot_longer(cols = c(Cobertura, AB_m2.ha, Media_altura, biomasa_mg.ha),
               names_to = "Attribute",
               values_to = "value") 

# Build boxplots --------------------

## First plot
p1 <- df  %>%
  # Filter data just to stay with basal area data
  filter(Attribute == "AB_m2.ha") %>%
  # Do ggplot
  ggplot(aes(x = tipo_bosque, 
             y = value)) + 
  # Add boxplot
  geom_boxplot(fill = "gray90", width = 0.6) +
  # Add threshold line
  geom_hline(yintercept = 8.02, col = "red", lty = "longdash")+
  # Add text for the threshold's value
  geom_text(aes(x = 1.5, y = 9, label = "8.02")) +
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
  filter(Attribute == "biomasa_mg.ha") %>%
  ggplot(aes(x = tipo_bosque, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) +
  geom_hline(yintercept = 23.3, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 26, label = "23.3"))+
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
  filter(Attribute == "Cobertura") %>%
  ggplot(aes(x = tipo_bosque, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) + 
  geom_hline(yintercept = 90.2, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 94, label = "90.2"))+
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
  filter(Attribute == "Media_altura") %>%
  ggplot(aes(x = tipo_bosque, 
             y = value)) + 
  geom_boxplot(fill = "gray90", width = 0.6) + 
  geom_hline(yintercept = 4.88, col = "red", lty = "longdash")+
  geom_text(aes(x = 1.5, y = 5.15, label = "4.88"))+
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
save_plot("Final_boxplot.jpg", p_final, 
          base_height = 8,
          base_asp = 0.85)

