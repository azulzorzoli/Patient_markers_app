library(ggplot2)
library(tidyverse)
library(ggh4x)
library(egg)
library(showtext)
library(rsvg)
library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)

# Read the data from the Excel file
df <- readxl::read_xlsx("sample_data.xlsx", sheet = "inflammatory_markers")

# Remove rows with missing platelet and wcc values
df <- df[!is.na(df$platelets) & !is.na(df$wcc), ]

# Convert the date column to the appropriate format
df$date <- as.Date(df$date)

# Set the start and end dates for the x-axis
axis_start_date <- min(df$date)
axis_end_date <- max(df$date)

# Create the plot with a more modern aesthetic
ggplot(df, aes(x = date)) +
  geom_point(aes(y = platelets), color = "#0072B2", size = 2) +
  geom_path(aes(y = platelets), color = "#0072B2", linewidth = 1) +
  geom_point(aes(y = wcc * 14), color = "#D55E00", size = 2) +
  geom_line(aes(y = wcc * 14), color = "#D55E00", linewidth = 1) +
  labs(x = "Date", y = "Platelets (x10^9/L)") +
  scale_x_date(
    limits = c(axis_start_date, axis_end_date),
    breaks = "week",
    date_labels = "%d %b",
    minor_breaks = "day"
  ) +
  scale_y_continuous(
    limits = c(0, max(df$platelets)),
    sec.axis = sec_axis(trans = ~./14, name = expression(paste("WCC (x10^9/L)")))
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(size = 12, face = "bold", vjust = 0.5),
    axis.title.y.right = element_text(size = 12, vjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(1.2, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
