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

scenario_choices <- c("One Group, Categorical", "One Group, Quantitative", 
                      #"Two Groups, Categorical", "Two Groups, Quantitative", 
                      #"Matched Pairs", "Two Quantitative Variables"
                      "Predefined Scenario"
                      )
dataset_choices <- c()

default_quant_data <- rnorm(30, mean = 60, sd = 5) %>%
  round() %>%
  paste(collapse = "\n")

ui <- fluidPage(
  "Visual Intuition",
  fluidRow(
    column(
      width = 3,
      selectInput("type", "Select Data Type", choices = scenario_choices, selected = 1),
      conditionalPanel(
        "input.type=='One Group, Categorical'",
        numericInput("successes", "# Successes", value = 0, min = 0, max = Inf, step = 1),
        numericInput("total", "Total Trials", value = 10, min = 1, max = Inf, step = 1),
        hr(),
        numericInput("pi", "Population proportion π = ", value = .5, min = 0, max = 1, step = .01)
      ), 
      conditionalPanel(
        "input.type=='One Group, Quantitative'",
        textAreaInput("sample", "Sample observations (one per line, must be a number)", value = default_quant_data),
        hr(),
        numericInput("mu", "Population Mean μ = ", value = 55, min = -Inf, max = Inf, step = .1),
        numericInput("sigma", "Population sd σ =", value = 5, min = .1, max = Inf, step = .1),
        p("You can get these from the one-mean app if necessary"),
        hr(),
        radioButtons("geom1quant", "Display Type", 
                     choiceNames = c("Histogram", "Density"),
                     choiceValues = c("geom_histogram", "geom_density"))
      ), 
      conditionalPanel(
        "input.type=='Predefined Scenario'",
        p("This shows the Old Faithful Eruptions data in one panel, and distributions with similar mean and variance in the other panels. Can you spot the eruptions data?")
      )
    ),
    column(
      width = 9,
      plotOutput("lineup")
    )
  )
)

server <- shinyServer(
  function(input,output,session) {
    
    relevant_geom <- reactive({
      if (input$type == "One Group, Categorical") {
        list(geom_bar(aes(x = x, fill = x)), scale_x_discrete(drop = F))
      } else if (input$type == "One Group, Quantitative") {
        if (input$geom1quant == "geom_histogram") {
          geom_histogram(aes(x = x)) }
        else {
          geom_density(aes(x = x))
        }
      } else {
        geom_histogram(aes(x = x))
      }
    })
    
    format_data <- reactive({
      if (input$type == "One Group, Categorical") {
        tibble(x = input$successes)
      } else if (input$type == "One Group, Quantitative") {
        tibble(
          x = str_split(input$sample, "[\\n\\r]", simplify = T) %>% parse_number()
        )
      } else {
        data(faithful)
        tibble(
          x = faithful$eruptions
        )
      }
    })
    
    target_location <- reactiveVal()
    
    lineup_data <- reactive({
      # Depend on all inputs
      list(input$mu, input$sigma, input$pi, input$total, input$sample, input$successes)
      
      # Pick a new target location
      target_location(sample(1:20, size = 1))
      
      if (input$type == "One Group, Categorical") {
        validate(need(input$total > 0, "Total trials must be > 0"))
        data <- lineup(method = null_dist(var = "x", dist = "binomial", 
                                          params = list(prob = input$pi, 
                                                        size = input$total)), 
                       n = 20, 
                       true = format_data(), pos = target_location()) %>%
          mutate(orig = x, 
                 x = purrr::map2(
                   orig, input$total, 
                   ~tibble(x = c(rep("success", .x), 
                                 rep("failure", .y - .x)) %>%
                             factor(levels = c("failure", "success"))))) %>% 
          unnest("x")
      } else if (input$type == "One Group, Quantitative") {
        data <- lineup(method = null_dist("x", "normal", 
                                          params = list(mean = input$mu, 
                                                        sd = input$sigma)),
                       n = 20, 
                       true = format_data(), pos = target_location())
      } else {
        data <- lineup(method = null_dist("x", "normal", 
                                          params = list(mean = 3.488, 
                                                        sd = 1.141)),
                       n = 20,
                       true = format_data(), pos = target_location())
      }
      
      print(head(data))
      
      data
    })
    
    output$lineup <- renderPlot({
      df <- lineup_data()
      
      
      ggplot(data = df) + 
        facet_wrap(~.sample) +
        relevant_geom()
    })
  })


# Run the application 
shinyApp(ui = ui, server = server)
