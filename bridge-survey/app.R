# Load data and libraries -------------------------------------------
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

# Read in Data
df <- read_csv('data/bcc-bridge-survey-final.csv')

# UI Elements
fluid_row_1 <- fluidRow(
  valueBoxOutput("total_responses"),
  valueBoxOutput("average_acceptance"),
  valueBoxOutput("average_trust")
)

fluid_row_2 <- fluidRow(
  column(6,
         plotlyOutput("acceptance")
  ),
  column(6,
         plotlyOutput("trust")
  )
)

fluid_row_3 <- fluidRow(
  column(6,
         plotlyOutput("social_infrastructure")
  ),
  column(6,
         plotlyOutput("procedural_fairness")
  )
)

fluid_row_4 <- fluidRow(
  column(6,
         plotlyOutput("contact_quality")
  ),
  column(6,
         plotlyOutput("contact_quantity")
  )
)

header <- dashboardHeader(title = 'Brisbane City Council')
sidebar <- dashboardSidebar(
)
body <- dashboardBody(
  h2("BCC SLO Monitor"),
  fluid_row_1, 
  h2("Key Performance Indicators"), 
  fluid_row_2,
  fluid_row_3,
  fluid_row_4
)

# User Interface -----------------------------------------------------
ui <- dashboardPage(
  header, sidebar, body
)

server <- function(input, output) {
  # Get the number of responses to the survey
  average_acceptance <- mean(df$question_10_enumerate)
  responseNumber <- nrow(df)
  average_trust <- mean(df$question_9_enumerate)
  
  q1_df <- df[ which( df$`Recorded_Date` == 'Q1') , ]
  q2_df <- df[ which( df$`Recorded_Date` == 'Q2'), ]
  q3_df <- df[ which( df$`Recorded_Date` == 'Q3'), ]
  
  x1 <- c('Q1', 'Q2', 'Q3')

  #Q1 aggregations
  q1_trust = mean(q1_df$question_9_enumerate)
  q1_acceptance = mean(q1_df$question_10_enumerate)
  q1_contact_quantity = (mean(q1_df$question_1_enumerate) + mean(q1_df$question_2_enumerate))/2
  q1_contact_quality = (mean(q1_df$question_4_enumerate) + mean(q1_df$question_5_enumerate))/2
  q1_social_infrastructure = mean(q1_df$question_3_enumerate)
  q1_procedural_fairness = (mean(q1_df$question_6_enumerate) + mean(q1_df$question_7_enumerate) + mean(q1_df$question_8_enumerate))/3
  
  #Q2 aggregations
  q2_trust = mean(q2_df$question_9_enumerate)
  q2_acceptance = mean(q2_df$question_10_enumerate)
  q2_contact_quantity = (mean(q2_df$question_1_enumerate) + mean(q2_df$question_2_enumerate))/2
  q2_contact_quality = (mean(q2_df$question_4_enumerate) + mean(q2_df$question_5_enumerate))/2
  q2_social_infrastructure = mean(q2_df$question_3_enumerate)
  q2_procedural_fairness = (mean(q2_df$question_6_enumerate) + mean(q2_df$question_7_enumerate) + mean(q2_df$question_8_enumerate))/3
  
  #Q3 aggregations
  q3_trust = mean(q3_df$question_9_enumerate)
  q3_acceptance = mean(q3_df$question_10_enumerate)
  q3_contact_quantity = (mean(q3_df$question_1_enumerate) + mean(q3_df$question_2_enumerate))/2
  q3_contact_quality = (mean(q3_df$question_4_enumerate) + mean(q3_df$question_5_enumerate))/2
  q3_social_infrastructure = mean(q3_df$question_3_enumerate)
  q3_procedural_fairness = (mean(q3_df$question_6_enumerate) + mean(q3_df$question_7_enumerate) + mean(q3_df$question_8_enumerate))/3
  
  x1 <- c('Q1', 'Q2', 'Q3')
  trust <-c(q1_trust, q2_trust, q3_trust)
  acceptance <-c(q1_acceptance, q2_acceptance, q3_acceptance)
  contact_quality <- c(q1_contact_quality, q2_contact_quality, q3_contact_quality)
  contact_quantity <- c(q1_contact_quantity, q2_contact_quantity, q3_contact_quantity)
  social_infrastructure <- c(q1_social_infrastructure, q2_social_infrastructure, q3_social_infrastructure)
  procedural_fairness <- c(q1_procedural_fairness, q2_procedural_fairness, q3_procedural_fairness)
  
  output$total_responses <- renderValueBox({
    valueBox(
      formatC(responseNumber, format = "d", digits=1, big.mark = ','),
      subtitle = "Survey Responses",
      color = "green")
  })
  output$average_acceptance <- renderValueBox({
    valueBox(
      formatC(q3_acceptance, format = "f", digits=1, big.mark = ','),
      subtitle = "Current Acceptance",
      color = "blue"
    )
  })
  output$average_trust <- renderValueBox({
    valueBox(
      formatC(q3_trust, format = "f", digits=1, big.mark = ','),
      subtitle = "Current Trust",
      color = "purple"
    )
  })
  
  y_axis_list <- list(
    title = "Mean",
    autotick = FALSE,
    ticks = "outside",
    tick0 = 0,
    dtick = 0.5,
    ticklen = 5,
    rangemode = 'tozero',
    family = "Old Standard TT, serif",
    size = 14,
    color = "black"
  )
  
  #plotly chart of acceptance
  output$acceptance <- renderPlotly({
      plot_ly(x = ~x1, y = ~acceptance, type = "scatter", mode = "lines", color = I('dark green'), name = "Acceptance",  width=500,
              height=350, tick0 = 0,
              dtick = 0.25,
              ticklen = 5,) %>%
      layout(title = "Acceptance",
             
             xaxis = list(title = "Year"),
             yaxis = y_axis_list)
  })
  
  #plotly chart of Trust
  output$trust <- renderPlotly({
    plot_ly(x = ~x1, y = ~trust, type = "scatter", mode = "lines", color = I('dark green'), name = "Trust", width=500,
            height=350) %>%
      layout(title = "Trust",
             title_font_family="Arial",
             xaxis = list(title = "Year"),
             yaxis = y_axis_list)
  })
  
  #plotly chart of Social Infrastructure
  output$social_infrastructure <- renderPlotly({
    plot_ly(x = ~x1, y = ~social_infrastructure, type = "scatter", mode = "lines", color = I('dark green'), name = "Social Infrastructure", width=500,
            height=350) %>%
      layout(title = "Social Infrastructure",
             title_font_family="Arial",
             xaxis = list(title = "Year"),
             yaxis = y_axis_list)
  })
  
  output$contact_quality <- renderPlotly({
    plot_ly(x = ~x1, y = ~contact_quality, type = "scatter", mode = "lines", color = I('dark green'), name = "Contact Quality", width=500,
            height=350) %>%
      layout(title = "Contact Quality",
             title_font_family="Arial",
             xaxis = list(title = "Year"),
             yaxis = y_axis_list
    )
  })
  

  
  output$contact_quantity <- renderPlotly({
    plot_ly(x = ~x1, y = ~contact_quantity, type = "scatter", mode = "lines", color = I('dark green'), name = "Contact Quantity", width=500,
            height=350) %>%
      layout(title = "Contact Quantity",
             title_font_family="Arial",
             xaxis = list(title = "Year"),
             yaxis = y_axis_list)
  })
  
  output$procedural_fairness <- renderPlotly({
    plot_ly(x = ~x1, y = ~procedural_fairness, type = "scatter", mode = "lines", color = I('dark green'), name = "Procedural Fairness", width=500,
            height=350) %>%
      layout(title = "Procedural Fairness",
             title_font_family="Arial",
             xaxis = list(title = "Year"),
             yaxis = y_axis_list)
  })
  
}

# Create shiny application ------------------------------------------
shinyApp(ui, server)