library(tidyverse)
library(shiny)
library(shinythemes)

# Load the data.
crime_data <- read.csv("ucr_crime_1975_2015.csv")

# Convert years to integers.
crime_data$year <- as.integer(crime_data$year)

# Keep only rows when 12 months were reported.
crime_data <- crime_data %>% filter(months_reported == 12)

# Make column names nicer.
crime_data <- crime_data %>%  mutate(Homicide = homs_per_100k,
                                     Robbery = rob_per_100k,
                                     Assault = agg_ass_per_100k,
                                     Rape = rape_per_100k,
                                     Violence = violent_per_100k)

# Define UI for application that draws a line chart.
shinyUI(fluidPage(theme=shinytheme("united"),
                  titlePanel("Crime rates across US police departments, 1975-2015"),
                  #mainPanel("Data collected by the Marshall Project: https://github.com/themarshallproject/city-crime",
                  plotOutput("crime_plot"),
                  br(),
                  fluidRow(
                          column(3,
                                 selectInput('my_y_choice',
                                             label = 'Type of crime:',
                                             choices = c("Violence", "Homicide", "Robbery", "Assault", "Rape")),
                                 selectInput('baseline_department',
                                             label = 'Baseline Department:',
                                             choices = unique(crime_data$department_name)),
                                 selectInput('comparison_department',
                                             label = 'Comparison Department:',
                                             choices = unique(crime_data$department_name)),
                                 sliderInput("years",
                                             label = "Year range:",
                                             min=1975,
                                             max=2015,
                                             value=c(1975,2015),
                                             step = 1,
                                             round = FALSE)
                                 ),
                          column(3,
                                 radioButtons("abs_or_rel",
                                              "Scale:",
                                              choices = c("Absolute", "Relative"),
                                              selected = "Absolute",
                                              inline = FALSE,
                                              width = NULL),
                                 checkboxInput("raw_or_norm",
                                               label = "Show raw counts",
                                               value = FALSE),
                                 checkboxInput("country_ave",
                                               label = "Show average",
                                               value = FALSE),
                                 sliderInput("alpha",
                                             label = "Visibility of background trends:",
                                             min=0,
                                             max=0.4,
                                             value=0.2,
                                             step = 0.05,
                                             round = FALSE),
                                 tags$button(
                                         id = 'close',
                                         type = "button",
                                         class = "btn action-button",
                                         onclick = "window.close()",
                                         "Close"
                                 )
                                 )
                  )
        )
)
