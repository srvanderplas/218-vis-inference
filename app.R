#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(nullabor)

load("isi_data.Rdata")
ui <- fluidPage(
    "Visual Intuition",
        column(
            width = 3,
            wellPanel(
                selectInput(inputId = "ch", label = "Chapter", choices = c("P", "1", "2", "3", "5", "6", "7", "10"), selected = 2),
                selectInput(inputId = "data", label = "Data", choices = isi_data$value, selected = 1),
                
            ))
)

server <- shinyServer(
    function(input,output,session) {
        observe({
            if(length(input$ch) > 0) {
                ch_str <- sprintf("(Example|Exploration) %s.", input$ch)
                df <- dplyr::filter(isi_data, grepl(ch_str, isi_data$chapter))
                updateSelectInput(session, "data", choices = df$value)
            }
        })
        
        create_lineup_data <- reactive({
            df <- dplyr::filter(isi_data, value == input$data)$data
            if (ncol(df) == 1) {
                # Fit normal distribution parameters and sample from that
                method <- null_dist(var = names(df)[1], dist = "norm")
                solution <- sample(1:20, 1)
                tmp <- lineup(method, true = df[,1], n = 20, pos = solution)
                plot_command <- geom_histogram(aes_string(x = names(df)[1]))
                list(data = tmp, answer = solution, geom = plot_command)
            } else if (ncol(df) == 2) {
                # One character, one numeric
                data_types <- map_chr(df, mode)
                if (all(as.character(data_types) == c("character", "numeric"))) {
                    method <- null_permute(names(df)[data_types=="character"])
                    solution <- sample(1:20, 1)
                    tmp <- lineup(method, true = df, n = 20, pos = solution)
                    plot_command <- geom_histogram(aes_string(x = names(df)[data_types == "numeric"],
                                                              fill = names(df)[data_types == "character"]))
                    list(data = tmp, answer = solution, geom = plot_command)
                }
                    
            }
        })
    })


# Run the application 
shinyApp(ui = ui, server = server)
