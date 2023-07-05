library(shiny)
library(ggplot2)
library(tidyverse)
library(ggh4x)
library(egg)
library(showtext)
library(rsvg)
library(shiny)
library(shinythemes)
library(shiny)
library(shinythemes)
library(plotly)




# Creating a user interface (UI) using the Shiny package
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Apply a predefined theme for a professional look (you can choose different themes)
  titlePanel("Antimicrobial Therapy and Inflammation Markers"),  # Title of the application
  br(), br(),  # Add two line breaks for empty space
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Adjust the width of the sidebar panel (smaller value makes it narrower)
      tags$h3("Upload Data"),  # Header for the sidebar panel
      fileInput("data_file", "Choose Data File"),  # Input file for data upload
      br(),  # Add some space between elements
      downloadButton("download_data", "Download Sample Data", class = "btn-primary btn-block")  # Button to download sample data
    ),
    mainPanel(
      tableOutput("patient_details_table"),  # Output table for patient details
      plotOutput("gantt_chart", height = "700px")  # Output plot for Gantt chart with adjusted height
    ),
  )
)




# Server function

server <- function(input, output) {
  # Reactive expression for the uploaded data
  uploaded_data <- reactive({
    req(input$data_file)
    readxl::read_excel(input$data_file$datapath)
  })
  
  
  
  # Summary of the uploaded data
  data_summary <- reactive({
    summary(uploaded_data())
  })
  
  # Render the data summary as a table
  output$data_summary <- renderPrint({
    data_summary()
  })
  
  output$gantt_chart <- renderPlot({
    # Ensure environment is clear
    rm(list = ls())
    
    
    showtext_auto()
    # set data file to be imported using pop-up file chooser
    data_file <- input$data_file$datapath
    
    
    
    # load font depending on operating system in use; turn on showtext
    ifelse(
      Sys.info()['sysname'] == "Darwin",
      font_add("Courier New",
               regular = "/System/Library/Fonts/Supplemental/Courier New.ttf",
               bold = "/System/Library/Fonts/Supplemental/Courier New Bold.ttf"),
      ifelse(
        Sys.info()['sysname'] == "Linux",
        font_add("Courier New",
                 regular = "/usr/share/fonts/truetype/msttcorefonts/Courier_New.ttf",
                 bold = "/usr/share/fonts/truetype/msttcorefonts/Courier_New_Bold.ttf"),
        font_add("Courier New",
                 regular = "C:/Windows/Fonts/cour.ttf",
                 bold = "C:/Windows/Fonts/courbd.ttf")
      )
    )
    
    showtext_auto()
    
    # Read the uploaded data instead of the fixed file
    therapy <- uploaded_data()
    
    # import data
    therapy <- readxl::read_excel(data_file, sheet = "therapy")
    inflammatory_markers <- readxl::read_excel(data_file,
                                               sheet = "inflammatory_markers",
                                               col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric"))
    cultures <- readxl::read_excel(data_file, sheet = "cultures")
    
    # create antibiotic factor levels based on minimum start date
    antibiotic_levels <- therapy %>%
      select(start, end, antibiotic) %>%
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
      mutate(time = replace_na(time, "00:00")) %>%
      unite("date_time", c("date", "time"), sep = " ") %>%
      mutate(date_time = ymd_hm(date_time),
             wcc = wcc * 14) %>% # rescale WCC for secondary y-axis
      pivot_longer(
        cols = c("crp", "wcc", "temp", "creatinine"),
        names_to = "marker",
        values_to = "value"
      ) %>%
      drop_na()
    
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
    x_axis_start_date <- min(c(therapy$date, inflammatory_markers$date_time))
    x_axis_end_date <- max(c(therapy$date, inflammatory_markers$date_time))
    
    # if date axis >= 6 months adjust labels, major and minor breaks
    x_axis_length <- x_axis_start_date %--% x_axis_end_date %/% months(1)
    
    if(x_axis_length >= 6) {
      breaks_value <- "month"
      minor_breaks_value <- "month"
      date_labels_value <- "%b %y"} else {
        breaks_value <- "week"
        minor_breaks_value <- "day"
        date_labels_value <- "%d %b"}
    
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
        limits = c(x_axis_start_date, x_axis_end_date),
        breaks = breaks_value,
        minor_breaks = minor_breaks_value
      )+
      labs(x = NULL,
           y = NULL) +
      scale_colour_viridis_d() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      )
    
    # plot fever curve# plot fever curve
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
      scale_x_datetime(
        limits = as_datetime(c(x_axis_start_date, x_axis_end_date)),
        breaks = breaks_value,
        minor_breaks = minor_breaks_value
      ) +
      scale_y_continuous(
        limits = c(35.0, 40.0),
        expand = c(0, 0),
        oob = scales::oob_keep,
        labels = function(x) {
          ifelse(x >= 40, expression(paste("\u2265 40")), as.character(x))
          ifelse(x <= 35, expression(paste("\u2264 35")), as.character(x))
        }
      ) +
      labs(
        x = NULL,
        y = expression(paste("Temperature (", degree, "C)"))
      ) +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 8, angle = 0, hjust = 1, vjust = 0.5),
        legend.position = "none",
        panel.grid.major = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
      )
    
    
    # plot laboratory markers
    p3_temp <- inflammatory_markers %>%
      filter(marker != "temp") %>%
      mutate(marker = str_replace_all(marker, c("creatinine" = "Creatinine",
                                                "crp" = "CRP",
                                                "wcc" = "WCC"))) %>%
      ggplot(aes(x = date_time)) +
      geom_pointpath(aes(y = value, colour = marker, shape = marker), size = 1) +
      coord_cartesian(expand = TRUE, clip = "on") +
      scale_shape_manual(values = c(1, 4, 5)) +
      scale_colour_brewer(palette = "Set2") +
      scale_x_datetime(
        limits = as_datetime(c(x_axis_start_date, x_axis_end_date)),
        breaks = breaks_value,
        date_labels = date_labels_value,
        minor_breaks = minor_breaks_value
      ) +
      labs(
        x = NULL,
        y = ifelse(
          "creatinine" %in% inflammatory_markers$marker,
          "CRP (mg/L) /\nCreatinine (µmol/L)",
          "CRP (mg/L)"
        )
      ) +
      theme(
        axis.title.y = element_text(size = 8, angle = 0, hjust = 1, vjust = 0.5),
        axis.title.y.right = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.justification = "bottom"
      )
    
    
    # scale y-axis and sec_y_axis in p3 depending on whether wcc present
    ifelse(
      "wcc" %in% inflammatory_markers$marker,
      p3 <- p3_temp +
        scale_y_continuous(
          limits = c(0, max(inflammatory_markers$value)),
          sec.axis = sec_axis(trans = ~ . / 14,
                              name = expression(paste("WCC (x1", 0 ^ 9, "/L)")))
        ),
      p3 <- p3_temp +
        scale_y_continuous(limits = c(0, max(inflammatory_markers$value)))
    )
    
    p4 <- ggplot(cultures, aes(x = date, y = organism, shape = organism, color = organism, text = paste("Date: ", date, "<br>Specimen: ", specimen))) +
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
    plotly_plot <- ggplotly(p4, tooltip = "text")
    
    # Customize the plot layout using Plotly's layout() function
    plotly_plot <- plotly_plot %>% layout(
      legend = list(orientation = "h", x = 0.25, y = -0.25)  # Move the legend further down by 1 cm
    )
    
    # Display the interactive plot
    plotly_plot
    
    
    
    
    
    # Join plots
    # If no temperature data, it doesn't get plotted
    if ("temp" %in% inflammatory_markers$marker) {
      p_final <- egg::ggarrange(p1, p2, p3, p4, ncol = 1)
    } else {
      p_final <- egg::ggarrange(p1, p3, p4, ncol = 1)
    }
    
    # show different plot areas using expose_layout()
    # pl <- lapply(list(p1, p2, p3), expose_layout, FALSE, FALSE)
    # grid.arrange(
    #   grobs = pl,
    #   heights = c(1, 1, 2),
    #   layout_matrix = cbind(c(1, 2, 3))
    # )
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)