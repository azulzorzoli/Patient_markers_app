library(ggplot2)
library(tidyverse)
library(ggh4x)
library(egg)
library(showtext)
library(rsvg)
library(readxl)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(lubridate)


# Read the data from the file "test_2"
cultures <- read_excel("test_2.xlsx", sheet = "cultures")

# Remove rows with NA values in the organism column
cultures <- cultures[complete.cases(cultures$organism), ]

# Create the interactive plot using plotly
plot <- ggplot(cultures, aes(x = date, y = organism, shape = organism, color = organism, text = paste("Date: ", date, "<br>Specimen: ", specimen))) +
  geom_point(size = 4) +
  labs(x = "Date", y = "") +  # Remove the y-axis label
  theme_bw() +
  theme(
    text = element_text(family = "Courier New", face = "bold"),
    panel.grid.major = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.3, linetype = 0),
    axis.text.y = element_blank(),  # Remove the labels from the y-axis
    legend.position = "top",  # Move the legend to the top
    legend.box = "horizontal"    # Set legend orientation to horizontal
  ) +
  guides(
    shape = guide_legend(title.position = "top", nrow = 1),
    color = guide_legend(title.position = "top", nrow = 1)
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(b = 2.5, unit = "cm"))

# Convert the plot to a plotly object
plotly_plot <- ggplotly(plot, tooltip = "text")

# Customize the plot layout using Plotly's layout() function
plotly_plot <- plotly_plot %>% layout(
  legend = list(orientation = "h", x = 0.25, y = -0.25)  # Move the legend further down by 1 cm
)

# Display the interactive plot
plotly_plot


























#plot without specimen information####
# Read the data from the file "test_2"
cultures <- read_excel("test_2.xlsx", sheet = "cultures")

# Remove rows with NA values in the organism column
cultures <- cultures[complete.cases(cultures$organism), ]

# Plot the organisms found per day with different colors and shapes
ggplot(cultures, aes(x = date, y = organism, shape = organism, color = organism)) +
  geom_point(size = 4) +
  labs(x = "Date", y = "Organism", shape = "Organism", color = "Organism") +
  theme_bw() +
  theme(
    text = element_text(family = "Courier New", face = "bold"),
    panel.grid.major = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    legend.position = "bottom"  # Move the legend to the bottom
  ) +
  guides(shape = guide_legend(title.position = "top", nrow = 1),
         color = guide_legend(title.position = "top", nrow = 1)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(b = 2.5, unit = "cm"))  # Adjust the bottom margin



#Original plot####

# Read the data from the file "test_2"
cultures <- read_excel("test_2.xlsx", sheet = "cultures")

# Remove rows with NA values in the specimen and organism columns
cultures <- cultures[complete.cases(cultures$specimen) & complete.cases(cultures$organism), ]

# Plot the organisms found per day and type of specimen
ggplot(cultures, aes(x = date, y = organism, shape = organism, color = specimen)) +
  geom_point(size = 4) +
  labs(x = "Date", y = "Organism", shape = "Organism", color = "Specimen Type") +
  theme_bw() +
  theme(
    text = element_text(family = "Courier New", face = "bold"),
    panel.grid.major = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    legend.position = "bottom"  # Move the legend to the bottom
  ) +
  guides(shape = guide_legend(title.position = "top", nrow = 1),
         color = guide_legend(title.position = "top", nrow = 1)) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(b = 2.5, unit = "cm"))  # Adjust the bottom margin

