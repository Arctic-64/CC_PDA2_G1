library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(tidyverse)
library(here)


####################################################################
###############
###############  Simone Famiano

dashboard_data_table <- read_csv(here("clean_data/dashboard_data_table.csv"))
tot_and_avg_stays <- read_csv(here("clean_data/national_total_stays_and_avg_length.csv"))

####################################################################
###############
###############  John Hios
# Load data
kpi_beds <- read_csv(here("clean_data/kpi_beds_clean.csv")) %>%
  mutate(across(is.numeric, round, digits=1))  

kpi_diff_beds <- read_csv(here("clean_data/kpi_diff_beds_clean.csv")) %>%
  mutate(across(is.numeric, round, digits=1)) 

# Get drop-down list menu options (and delete any occurring NAs)
hb_name_labels <- unique(kpi_beds$HBName) %>% 
  discard(is.na)


kpi_labels <- c("All Staffed Beds", 
                "Total Occupied Beds", 
                "Average Available Staffed Beds", 
                "Average Occupied Beds", 
                "Percentage Occupancy")

####################################################################




ui <- fluidPage(
  tabsetPanel(
    
    tags$head(tags$style('
    body {
      font-family: Arial; 
      font-size: 20px; 
    }'
    )),
   
    
    tabPanel("Demographics",
             
             
             
             # Application title
             
             h1(strong("Demographics Data"), align="center", style = "font-size:100px;"),
             
            
   
   br(),
   br(),
   
   fluidRow(  
     column(3,
            
            
            
            
            pickerInput("age_input", 
                        "Select Age Groups", 
                        choices = unique(tot_and_avg_stays$age),
                        multiple =  TRUE,
                        selected = unique(tot_and_avg_stays$age)),
     ),
     
     
     
     column(9,
            pickerInput("quarter_input", 
                        "Select Quarter", 
                        choices = unique(tot_and_avg_stays$quarter),
                        multiple =  TRUE,
                        selected = unique(tot_and_avg_stays$quarter))
            
     )
   ),
   
   
   plotOutput("tot_stays_plot"),
   
   br(),
   br(),
   
   
   plotOutput("avg_length_plot"),
   
   
   
   dataTableOutput("table_output"),
   
   
    ),
   
   tabPanel("Acute care vs. NHS bed numbers",
            h1(strong("Acute Care vs. NHS Bed Numbers"), align="center", style = "font-size:100px;"),
           
            tags$head(tags$style('
       body {
       font-family: Arial; 
       font-size: 16px;
       }'
            )),
       
       br(),
       br(),
       
       # ADD theme here
       # theme = shinytheme("united"),
       
       fluidRow(  
         column(6,
                selectInput("nhs_board_input",
                            "Select Board",
                            choices = hb_name_labels
                ),
         ),  
         
         column(6, 
                selectInput("kpi_input",
                            "Select Key Performance Index (KPI)",
                            choices = kpi_labels
                )
         ), 
         
       ),
       
       br(),
       br(),
       
       # Show a plot of the generated distribution
       plotOutput("trend_plot"),
       br(),
       br(),
       plotOutput("diff_plot")
       
   )   
   
  ))

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  
  ############### Simone's
  output$table_output <- renderDataTable({
    
    dashboard_data_table %>% 
      filter(Age %in% input$age_input)%>% 
      filter(Quarter %in% input$quarter_input) %>% 
      arrange(Location)
    
    datatable(dashboard_data_table, escape = FALSE, selection = "none") %>% 
      formatStyle("Gender",
                  target = "row",
                  backgroundColor = styleEqual(c("Female", "Male"),
                                               c("#062e3c", "#074859"))
      ) %>% 
      formatStyle(columns = names(dashboard_data_table), color = "white",
                  fontWeight = "bold", family = "Arial")
  })
  
  
  output$tot_stays_plot <- renderPlot({
    tot_and_avg_stays %>%
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>%
      ggplot() +
      geom_col(position = "dodge", colour = "white", aes(x = age, 
                                                         y = tot_stays, 
                                                         fill = quarter)
      ) +
      ggtitle("Total Stays") +
      labs(x = "Age", y = "Total Stays") +
      scale_fill_manual("Quarter", values = c("2020Q1" = "#061a1f",
                                              "2020Q2" = "#062e3c",
                                              "2020Q3" = "#074859",
                                              "2020Q4" = "#11667f",
                                              "2021Q1" = "#008b87",
                                              "2021Q2" = "#47899b",
                                              "2021Q3" = "#659799")
      ) +
      facet_wrap(~sex) +
      theme_bw() +
      theme(text=element_text(size = 20,  family = "Arial"),
            strip.background =element_rect(fill = "#062e3c"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  output$avg_length_plot <- renderPlot({
    tot_and_avg_stays %>% 
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>%
      select(age, average_length_of_stay, quarter, sex) %>% 
      ggplot() +
      geom_col(position = "dodge",  
               colour = "white", aes(x = age, y = average_length_of_stay, 
                                     fill = quarter)
      ) +
      ggtitle("Average Length of Stay") +
      labs(x = "Age", y = "Average Length of Stay") +
      scale_fill_manual("Quarter", values = c("2020Q1" = "#061a1f",
                                              "2020Q2" = "#062e3c",
                                              "2020Q3" = "#074859",
                                              "2020Q4" = "#11667f",
                                              "2021Q1" = "#008b87",
                                              "2021Q2" = "#47899b",
                                              "2021Q3" = "#659799")
      ) +
      facet_wrap(~sex) +
      theme_bw() +
      theme(text=element_text(size = 20,  family = "Arial"),
            strip.background =element_rect(fill = "#074859"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  
  ############### John's
  output$trend_plot <- renderPlot({
    
    kpi_beds %>%
      filter(HBName == input$nhs_board_input) %>%
      filter(Location == HB) %>%
      ggplot() +
      aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, fill = HBName) +
      geom_col(colour = "black") +
      geom_text(aes(label = .data[[input$kpi_input]]), size = 6, vjust = -0.25) +
      scale_fill_manual(guide = "none", values = "#008b87") +
      ylim(c(0, NA)) +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 20,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
  
  output$diff_plot <- renderPlot({
    
    kpi_diff_beds %>%
      filter(HBName == input$nhs_board_input) %>%
      filter(Location == HB) %>%
      ggplot() +
      aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, fill = HBName) +
      geom_col(colour = "black") +
      ylab("Pre-Covid19 Percentage Difference (%)") +
      geom_text(aes(label = .data[[input$kpi_input]]), size = 6, vjust = -0.25) +
      scale_fill_manual(guide = "none", values = "#008b87") +
      theme_bw() +
      theme(axis.title.x = element_text(margin = margin(t = 10)),
            text=element_text(size = 20,  family = "Arial"),
            strip.background = element_rect(fill = "#008b87"),
            strip.text = element_text(colour = 'white', size = 18,
                                      family = "Arial"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
