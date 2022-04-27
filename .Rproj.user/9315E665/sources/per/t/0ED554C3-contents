library(shiny)
library(tidyverse)
library(ggplot2)
library(CodeClanData)

games = CodeClanData::game_sales
ui <- fluidPage(
  sliderInput("year_of_release", "year_of_release", value = 2006, min = min(games$year_of_release) , max = max(games$year_of_release)),
           plotOutput("score")
)

server <- function(input, output) {
  
  filtered_df <- reactive({
    games %>%
      filter(year_of_release == input$year_of_release)
  })
  
  output$score <- renderPlot({
    ggplot(filtered_df()) +
      aes(x = critic_score, y = sales) +
      geom_col()
  })}

shinyApp(ui, server)

## this program visualises data across years. the slider alows for fast comparison.
## it shows the critics reviews in relation to the games sales.
## we can therfor gauge the relevenace of critic reviws incpredicting sales.