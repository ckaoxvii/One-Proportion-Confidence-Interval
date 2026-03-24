library(shiny)
library(bslib)
library(reactable)
library(tidyverse)

ui <- page_sidebar(
  title = "Confidence Interval Calculator for One Population",
  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  # Add custom CSS to ensure card headers have the correct styling
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #A90533 !important;
        color: white !important;
        font-weight: bold;
      }
    "))
  ),
  sidebar = sidebar(
    radioButtons(
      'input_type',
      'Input Type',
      choices = c(
        'Sample Proportion' = 'prop',
        'Number of Successes' = 'count'
      ),
      selected = 'prop'
    ),
    uiOutput('sample_input'),
    numericInput(
      'sample_size',
      "Sample Size:",
      value = 100
    ),
    numericInput(
      'conf_level',
      'Confidence Level (%)',
      value = 95
    )
  ),
  
  card(
    full_screen = TRUE,
    card_header("Results"),
    card_body(
      h4('Estimate of Population Proportion'),
      reactableOutput('table_1'),
      br(),
      h4('Confidence Interval'),
      reactableOutput('table_2')
    )
  )
)

server <- function(input, output, session) {

  output$sample_input <- renderUI({
    if (input$input_type == "prop") {
      numericInput(
        "sample_prop",
        "Sample Proportion:",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.01
      )
    } else {
      numericInput(
        "successes",
        "Number of Successes:",
        value = 50,
        min = 0,
        step = 1
      )
    }
  })

  p_hat <- reactive({
    if (input$input_type == 'prop') {
      req(input$sample_prop)
      validate(
        need(
          input$sample_prop >= 0 && input$sample_prop <= 1,
          'Sample proportion must be between 0 and 1.'
        )
      )
      input$sample_prop
    } else {
      req(input$successes)
      validate(
        need(input$successes >= 0, 'Number of successes must be nonnegative.'),
        need(input$successes <= input$sample_size, 'Number of successes cannot exceed the sample size')
      )
      input$successes/input$sample_size
    }
  })
  
  # calculate standard error
  standard_error <- reactive({
    req(p_hat(), input$sample_size)
    sqrt(p_hat() * (1 - p_hat()) / input$sample_size)
  })

  # calculate margin of error
  margin_of_error <- reactive({
    req(input$conf_level)
    alpha = 1 - input$conf_level / 100
    z_critical = qnorm(1 - alpha / 2)
    z_critical * standard_error()
  })

  # calculate CI lower bound
  lower_bound <- reactive({
    max(0, p_hat() - margin_of_error())
  })

  # calculate CI upper bound
  upper_bound <- reactive({
    min(1, p_hat() + margin_of_error())
  })
  
  # calculate confidence interval
  confidence_interval <- reactive({
    c(lower_bound(), upper_bound())
  })
  
  output$table_1 <- renderReactable({
    reactable(
      tibble(
        point_estimate = sprintf('%.4f', p_hat()),
        standard_error = sprintf('%.4f', standard_error()),
        margin_of_error = sprintf('%.4f', margin_of_error())
      ),
      defaultColDef = colDef(
        align = 'right'
      ),
      columns = list(
        point_estimate = colDef(name = 'Point Estimate'),
        standard_error = colDef(name = 'Standard Error'),
        margin_of_error = colDef(name = 'Margin of Error')
      )
    )
  })

  output$table_2 <- renderReactable({
    reactable(
      tibble(
        conf_level = paste0(input$conf_level, '%'),
        lower_bd = sprintf('%.4f', lower_bound()),
        upper_bd = sprintf('%.4f', upper_bound()),
      ),
      defaultColDef = colDef(
        align = 'right'
      ),
      columns = list(
        conf_level = colDef(name = 'Confidence Level'),
        lower_bd = colDef(name = 'Lower Bound'),
        upper_bd = colDef(name = 'Upper Bound')
      )
    )
  })

  # output interpretation
  output$interpretation <- renderText({
    ci <- confidence_interval()
    paste0(
      "We are ", input$conf_level,
      "% confident that the true population proportion lies between ",
      round(ci[1] * 100, 2), "% and ",
      round(ci[2] * 100, 2), "%."
    )
  })
}

shinyApp(ui = ui, server = server)
