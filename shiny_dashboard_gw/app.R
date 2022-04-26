library(shiny)
library(tidyverse)

data <- read_csv("clean_data/activity_by_board_of_treatment_and_speciality.csv")
hb_name <- unique(data$hb_name)
hb_urban <- unique(data$urban_rural)

ui <- fluidPage(
  
  titlePanel("group_project_geography"),
  
  fluidRow(
    column(6,
      selectInput(inputId = "hb_input", 
                  label = "Select NHS Health Board", 
                  choices = hb_name)),
    
    column(6,
      radioButtons(inputId = "urban_input", 
                   label = "Urban or rural?",
                   choices = hb_urban)),
      
    ),
    
    fluidRow(
      column(4,
      plotOutput("hb_activity")),
      
      column(4,
      plotOutput("hb_quarter")),
      
      column(4,
      plotOutput("hb_urban")),
    )
  )

server <- function(input, output) {
  
  output$hb_activity <- renderPlot({
    data %>% 
    filter(hb_name == input$hb_input) %>%
    group_by(quarter, admission_type) %>%  
    summarise(episodes = sum(episodes/1000)) %>% 
    ggplot() +
    geom_line(aes(x = quarter, y = episodes, group = admission_type, colour = admission_type), alpha = 1) +
    geom_point(aes(x = quarter, y = episodes, colour = admission_type), alpha = 1) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab("Admission type") +
    ylab("Thousands of episodes")})
  
  output$hb_quarter <- renderPlot({
    data %>%
    filter(hb_name == input$hb_input) %>%
    group_by(quarter, hb_name) %>%
    summarise(episodes = sum(episodes/total_occupied_beds)) %>%
    ggplot() +
    geom_line(aes(x = quarter, y = episodes, group = hb_name, colour = hb_name), alpha = 0.75) +
    geom_point(aes(x = quarter, y = episodes, colour = hb_name), alpha = 0.75) +
    xlab("Quarter") +
    ylab("Episodes") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))})
  
  output$hb_urban <- renderPlot({
    data %>%
      filter(hb_code != "S27000001", hb_code != "SN0811", hb_name != "Scotland") %>% 
      filter(urban_rural == input$urban_input) %>% 
      group_by(urban_rural, admission_type) %>% 
      summarise(episodes = sum(episodes/total_occupied_beds)) %>% 
      ggplot() +
      aes(x = admission_type, y = episodes) +
      geom_bar(stat = "identity") +
      facet_grid(~urban_rural) +
      xlab("Admission type") +
      ylab("Episodes") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))})
  
}

shinyApp(ui = ui, server = server)