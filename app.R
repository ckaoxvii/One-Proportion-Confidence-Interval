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
    numericInput(
      "sample_prop",
      "Sample Proportion:",
      value = 0.5
    ),
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
  
  # calculate standard error
  standard_error <- reactive({
    p_hat = input$sample_prop
    n = input$sample_size
    se = sqrt(p_hat * (1 - p_hat) / n)

    return(se)
  })

  # calculate margin of error
  margin_of_error <- reactive({
    se = standard_error()
    alpha = 1 - input$conf_level/100
    z_critical = qnorm(1 - alpha/2)
    margin_of_error = z_critical*se

    return(margin_of_error)
  })

  # calculate CI lower bound
  lower_bound <- reactive({
    lower_bound <- input$sample_prop - margin_of_error()

    return(lower_bound)
  })

  # calculate CI upper bound
  upper_bound <- reactive({
    upper_bound <- input$sample_prop + margin_of_error()

    return(upper_bound)
  })
  
  # calculate confidence interval
  confidence_interval <- reactive({
    p_hat <- input$sample_prop
    
    # Calculate confidence interval bounds
    lower_bound <- p_hat - margin_of_error()
    upper_bound <- p_hat + margin_of_error()
    
    # Ensure bounds are within [0, 1]
    lower_bound <- max(0, lower_bound)
    upper_bound <- min(1, upper_bound)
    
    return(c(lower_bound, upper_bound))
  })
  
  # output standard error
  output$standard_error <- renderText({
    se <- standard_error()
    paste0("SE = ", round(se, 6))
  })

  # output margin of error
  output$margin_of_error <- renderText({
    margin_of_error <- margin_of_error()
  })
  
  # output confidence interval
  output$confidence_interval <- renderText({
    ci <- confidence_interval()
    conf_level <- input$conf_level
    paste0(conf_level, "% CI: (", round(ci[1], 4), ", ", round(ci[2], 4), ")")
  })

  output$table_1 <- renderReactable({
    reactable(
      tibble(
        point_estimate = sprintf('%.4f', round(input$sample_prop, 4)),
        standard_error = sprintf('%.4f', round(standard_error(), 4)),
        margin_of_error = sprintf('%.4f', round(margin_of_error(), digits = 4))
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
        lower_bd = sprintf('%.4f', round(lower_bound(), 4)),
        upper_bd = sprintf('%.4f', round(upper_bound(), 4)),
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
    conf_level <- input$conf_level
    ci <- confidence_interval()
    paste0("We are ", conf_level, "% confident that the true population proportion ",
           "lies between ", round(ci[1], 4)*100, "%", " and ", round(ci[2], 4)*100, "%", ".")
  })
}

shinyApp(ui = ui, server = server)
