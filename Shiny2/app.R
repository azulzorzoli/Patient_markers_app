# plot fever curve
p2 <- inflammatory_markers %>%
  filter(marker == "temp") %>%
  ggplot(aes(x = date_time)) +
  geom_rect(
    aes(xmin = as_datetime(x_axis_start_date), xmax = as_datetime(x_axis_end_date),
        ymin = 35.1, ymax = 36.0),
    fill = alpha("yellow", 0.5),
    color = NA
  ) +
  geom_rect(
    aes(xmin = as_datetime(x_axis_start_date), xmax = as_datetime(x_axis_end_date),
        ymin = 36.1, ymax = 38.0),
    fill = alpha("white", 0.5),
    color = NA
  ) +
  geom_rect(
    aes(xmin = as_datetime(x_axis_start_date), xmax = as_datetime(x_axis_end_date),
        ymin = 38.1, ymax = 40),
    fill = alpha("yellow", 0.5),
    color = NA
  ) +
  geom_rect(
    aes(xmin = as_datetime(x_axis_start_date), xmax = as_datetime(x_axis_end_date),
        ymin = 39.1, ymax = 40),
    fill = alpha("orange", 0.5),
    color = NA
  ) +
  geom_pointpath(
    aes(y = value),
    shape = 1,
    size = 1,
    alpha = 0.8
  ) +
  geom_hline(
    yintercept = 38,
    color = "black",
    linetype = 3,
    linewidth = 0.3
  ) +
  coord_cartesian(
    ylim = c(35.0, 40.0),
    expand = FALSE,
    clip = "off"
  ) +
  scale_x_datetime(
    limits = as_datetime(c(x_axis_start_date, x_axis_end_date)),
    breaks = breaks_value,
    minor_breaks = minor_breaks_value
  ) +
  labs(
    x = NULL,
    y = expression(paste("Temperature (", degree, "C)"))
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 8, vjust = 7.5),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey", linetype = "dotted"),
    panel.grid.minor = element_line(color = "grey", linetype = "dotted"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )
