Test modifications of Ollie Lloyd's code 


# Antibiotic Gantt Chart
# Copyright (C) 2023  Ollie Lloyd
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#load packages
library(tidyverse)
library(ggh4x)
library(egg)
library(showtext)
library(rsvg)
library(readxl)


# Read the data from the file "test_2"
cultures <- read_excel("test_2.xlsx", sheet = "cultures")
therapy <- read_excel("test_2.xlsx", sheet = "therapy")
inflammatory_markers <- read_excel("test_2.xlsx", sheet = "inflammatory_markers")


# set data file
# excel spreadsheet should have two sheets:
# 1) therapy: ~start, ~end, ~antibiotic
# 2) inflammatory_markers: ~date, ~crp, ~wcc, ~temp, ~creatinine
# import data

# create antibiotic factor levels based on minimum start date
antibiotic_levels <- therapy %>%
  mutate(across(c("start", "end"), as_date)) %>%
  group_by(antibiotic) %>%
  slice_min(start) %>%
  arrange(start) %>%
  mutate(antibiotic = factor(antibiotic, levels = antibiotic)) %>%
  pull(antibiotic)

# wrangle data including reordering antibiotic factor level by start date
therapy <- therapy %>%
  mutate(id = 1:n(),
         across(c("start", "end"), as_date),
         antibiotic = factor(antibiotic, levels = rev(antibiotic_levels)),
         .before = 1) %>%
  pivot_longer(
    cols = c("start", "end"),
    names_to = "time_point",
    values_to = "date"
  )

inflammatory_markers <- inflammatory_markers %>%
  mutate(date = as_date(date),
         wcc = wcc * 14) %>% # rescale WCC for secondary y-axis
  pivot_longer(
    cols = c("crp", "wcc", "temp", "creatinine"),
    names_to = "marker",
    values_to = "value"
  ) %>%
  drop_na()

# Remove rows with NA values in the specimen and organism columns
cultures <- cultures[complete.cases(cultures$specimen) & complete.cases(cultures$organism), ]


# set theme for all plots
theme_set(theme_bw() +
            theme(
              text = element_text(family = "Courier New", face = "bold"),
              panel.grid.major = element_line(color = "grey",
                                              linewidth = 0.3,
                                              linetype = 3), 
              panel.grid.minor.x = element_line(color = "grey",
                                                linewidth = 0.3,
                                                linetype = 3)
            )
)

# generate start and end dates for x-axis
axis_start_date <- min(c(therapy$date, inflammatory_markers$date))
axis_end_date <- max(c(therapy$date, inflammatory_markers$date))

# plot antibiotic gantt chart
p1 <- therapy %>%
  ggplot(aes(x = date)) +
  geom_line(
    aes(y = antibiotic, colour = antibiotic, group = id),
    linewidth = 5,
    alpha = 0.8
  )+
  coord_cartesian(
    ylim = c(0.5, (n_distinct(therapy$antibiotic) + 0.5)),
    expand = FALSE,
    clip = "off"
  )+
  scale_x_date(
    limits = c(axis_start_date, axis_end_date),
    breaks = "week",
    minor_breaks = "day"
  )+
  labs(x = NULL,
       y = NULL) +
  scale_colour_viridis_d() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

# plot fever curve
p2 <- inflammatory_markers %>%
  filter(marker == "temp") %>%
  ggplot(aes(x = date)) +
  geom_pointpath(
    aes(y = value),
    shape = 1,
    size = 1,
    alpha = 0.8
  )+
  geom_hline(
    yintercept = 38,
    color = "black",
    linetype = 3,
    linewidth = 0.3
  )+
  coord_cartesian(
    ylim = c(35.5, 40.5), 
    expand = FALSE, 
    clip = "off"
  )+
  scale_x_date(
    limits = c(axis_start_date, axis_end_date),
    breaks = "week",
    minor_breaks = "day"
  ) +
  labs(x = NULL,
       y = expression(paste("Temperature (", degree, "C)"))) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 8,
                                vjust = 7.5),
    legend.position = "none"
  )

# plot laboratory markers
p3 <- inflammatory_markers %>%
  filter(marker != "temp") %>%
  mutate(marker = str_replace_all(marker, c("creatinine" = "Creatinine",
                                            "crp" = "CRP",
                                            "wcc" = "WCC"))) %>% 
  ggplot(aes(x = date)) +
  geom_pointpath(aes(y = value, colour = marker, shape = marker), size = 1) +
  coord_cartesian(expand = FALSE, clip = "off")+
  scale_shape_manual(values = c(1, 3, 5)) +
  scale_colour_brewer(palette = "Set2") +
  scale_x_date(
    limits = c(axis_start_date, axis_end_date),
    breaks = "week",
    date_labels = "%d %b",
    minor_breaks = "day"
  )+
  scale_y_continuous(
    limits = c(0, max(inflammatory_markers$value)),
    sec.axis = sec_axis(trans = ~ . / 14,
                        name = expression(paste("WCC (x1", 0 ^ 9, "/L)")))
  )+
  labs(
    x = NULL,
    y = ifelse(
      "creatinine" %in% inflammatory_markers$marker,
      "CRP (mg/L) / Creatinine (\u03BCmol/L)",
      "CRP (mg/L)"
    )
  )+
  theme(
    axis.title.y = element_text(size = 8,
                                face = "plain",
                                vjust = 5),
    axis.title.y.right = element_text(size = 8,
                                      vjust = 5),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.justification = "bottom"
  )

p4 <- ggplot(cultures, aes(x = date, y = organism, shape = organism, color = specimen)) +
  geom_point(size = 4) +
  labs(x = "Date", y = "Organism", shape = "Organism", color = "Specimen Type") +
  theme_bw() +
  theme(
    text = element_text(family = "Courier New", face = "bold"),
    panel.grid.major = element_line(color = "grey", linewidth = 0.3, linetype = 3),
    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.3, linetype = 3)
  )


# Join plots
# If no temperature data, it doesn't get plotted
if ("temp" %in% inflammatory_markers$marker) {
  p_final <- egg::ggarrange(p1, p2, p3, p4, ncol = 1)
} else {
  p_final <- egg::ggarrange(p1, p3, p4, ncol = 1)
}

# Display the final combined plot
print(p_final)

# convert svg file to pdf; bit of a fudge as cannot get font to export to
# postscript device which seems to be used for pdf when ggarrange is used
rsvg_pdf(
  paste0("output/", tools::file_path_sans_ext(basename(data_file)), ".svg"),
  paste0("output/", tools::file_path_sans_ext(basename(data_file)), ".pdf")
)
