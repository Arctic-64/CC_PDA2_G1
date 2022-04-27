library(shiny)
library(tidyverse)

data <- read_csv("../shiny_dashboard_gw/data/phs_admissions_data_clean.csv")
hb_name <- unique(data$hb_name)
hb_urban <- unique(data$urban_rural)

ui <- fluidPage(
  
  titlePanel(h1("National variation in hospital activity", align = "center")),
  
  fluidRow(
    
    titlePanel(h1("Health board", align = "center")),
    
    column(6,
           selectInput(inputId = "hb_input", 
                       label = "Select NHS Health Board", 
                       choices = hb_name)),
    
    column(6,
      selectInput(inputId = "hb_input", 
                  label = "Select NHS Health Board", 
                  choices = hb_name)),
    ),
  
  fluidRow(
    
    # column(6,
    #   radioButtons(inputId = "urban_input", 
    #                label = "Urban or rural?",
    #                choices = hb_urban)),
    
    column(5,
           plotOutput("hb_quarter")),
    
    column(7,
           plotOutput("hb_activity")),
    
    ),
    
  fluidRow(
    
    titlePanel(h1("Urban or rural", align = "center")),
    
    column(6,
      plotOutput("hb_urban")),
    
    column(6,
           plotOutput("hb_urban_rural_time")),
    
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
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")})
  
  output$hb_urban <- renderPlot({
    data %>%
      filter(hb_code != "S27000001", hb_code != "SN0811", hb_name != "Scotland") %>% 
      #filter(urban_rural == input$urban_input) %>% 
      group_by(urban_rural, admission_type) %>% 
      summarise(episodes = sum(episodes/total_occupied_beds)) %>% 
      ggplot() +
      aes(x = admission_type, y = episodes) +
      geom_bar(stat = "identity") +
      facet_grid(~urban_rural) +
      xlab("Admission type") +
      ylab("Episodes") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))})
  
  output$hb_urban_rural_time <- renderPlot({
    
    data %>%
      filter(hb_name != "National Facility NHS Louisa Jordan", hb_name != "Non-NHS Provider Location", hb_name != "The Golden Jubilee National Hospital") %>% # remove hb with pop NA
      filter(hb_code != "S27000001", hb_name != "National Facility NHS Louisa Jordan") %>% 
      group_by(quarter, urban_rural) %>% 
      summarise(episodes = sum(episodes)/mean(pop)) %>% 
      ggplot() +
      aes(x = quarter, y = episodes, group = urban_rural, colour = urban_rural) +
      geom_line() +
      ylab("Episodes per capita") +
      ylim(0,NA)
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)