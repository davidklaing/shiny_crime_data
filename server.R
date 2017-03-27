library(tidyverse)
library(shiny)
library(shinythemes)

# Load the data.
crime_data <- read.csv("ucr_crime_1975_2015.csv")

# Convert years to integers.
crime_data$year <- as.integer(crime_data$year)

# Keep only rows when 12 months were reported.
crime_data <- crime_data %>% filter(months_reported == 12)

# Define server logic.
shinyServer(function(input, output, session) {
        observe({
                
                # Keep only the years within the range selected by the user.
                crime_data <- crime_data %>% filter(year >= input$years[1], year <= input$years[2])
                
                # Select either raw counts or rates.
                if (input$raw_or_norm == FALSE) {
                        crime_data <- crime_data %>%  mutate(Homicide = homs_per_100k,
                                                             Robbery = rob_per_100k,
                                                             Assault = agg_ass_per_100k,
                                                             Rape = rape_per_100k,
                                                             Violence = violent_per_100k)
                        country_ave <- crime_data %>% group_by(year) %>% summarise(Homicide = mean(Homicide),
                                                                                   Robbery = mean(Robbery),
                                                                                   Assault = mean(Assault),
                                                                                   Rape = mean(Rape),
                                                                                   Violence = mean(Violence))
                        y_axis_label <- paste(input$my_y_choice, "per 100,000 people")
                } else {
                        crime_data <- crime_data %>%  mutate(Homicide = homs_sum,
                                                             Robbery = rob_sum,
                                                             Assault = agg_ass_sum,
                                                             Rape = rape_sum,
                                                             Violence = violent_crime)
                        country_ave <- crime_data %>% group_by(year) %>% summarise(Homicide = mean(Homicide),
                                                                                   Robbery = mean(Robbery),
                                                                                   Assault = mean(Assault),
                                                                                   Rape = mean(Rape),
                                                                                   Violence = mean(Violence))
                        y_axis_label <- paste("Raw counts of", input$my_y_choice)
                }
                
                # Select the baseline data.
                baseline_crime_data <- crime_data %>% filter(department_name == input$baseline_department)
                
                # Select the comparison data.
                comparison_crime_data <- crime_data %>% filter(department_name == input$comparison_department)
                
                # Scale relative to baseline data, if selected by the user.
                if (input$abs_or_rel == "Relative") {
                        comparison_crime_data <- inner_join(comparison_crime_data, baseline_crime_data, by = "year")
                        country_ave <- country_ave %>% mutate(Homicide = Homicide - baseline_crime_data$Homicide,
                                                              Robbery = Robbery - baseline_crime_data$Robbery,
                                                              Assault = Assault - baseline_crime_data$Assault,
                                                              Rape = Rape - baseline_crime_data$Rape,
                                                              Violence = Violence - baseline_crime_data$Assault)
                        crime_data <- inner_join(crime_data, baseline_crime_data, by = "year")
                        crime_data <- crime_data %>%  mutate(Homicide = Homicide.x - Homicide.y,
                                                             Robbery = Robbery.x - Robbery.y,
                                                             Assault = Assault.x - Assault.y,
                                                             Rape = Rape.x - Rape.y,
                                                             Violence = Violence.x - Violence.y,
                                                             department_name = department_name.x)
                        comparison_crime_data <- comparison_crime_data %>% mutate(Homicide = Homicide.x - Homicide.y,
                                                                                  Robbery = Robbery.x - Robbery.y,
                                                                                  Assault = Assault.x - Assault.y,
                                                                                  Rape = Rape.x - Rape.y,
                                                                                  Violence = Violence.x - Violence.y)
                        baseline_crime_data <- baseline_crime_data %>% mutate(Homicide = 0,
                                                                              Robbery = 0,
                                                                              Assault = 0,
                                                                              Rape = 0,
                                                                              Violence = 0)
                }
                
                # Determine whether the national average will be shown.
                if (input$country_ave == TRUE) {
                        country_ave_alpha = 1
                } else {
                        country_ave_alpha = 0
                }
                
                # Plot the data.
                output$crime_plot <- renderPlot({
                        ggplot(crime_data) +
                                geom_line(aes_string(x = crime_data$year, y = input$my_y_choice, fill = crime_data$department_name), alpha = input$alpha) +
                                geom_line(data = country_ave, aes_string(x = country_ave$year, y = input$my_y_choice), size = 1.1, alpha = country_ave_alpha) +
                                geom_line(data = comparison_crime_data, aes_string(x = comparison_crime_data$year, y = input$my_y_choice), color = "red", size = 1.1) +
                                geom_line(data = baseline_crime_data, aes_string(x = baseline_crime_data$year, y = input$my_y_choice), color = "blue", size = 1.1) +
                                theme(plot.subtitle = element_text(vjust = 1), 
                                      plot.caption = element_text(vjust = 1),
                                      axis.text=element_text(size=14),
                                      axis.title=element_text(size=14,face="bold")) +
                                labs(x = "Year", y = y_axis_label)
                })
                if (input$close > 0) stopApp()
        }
        )
})
