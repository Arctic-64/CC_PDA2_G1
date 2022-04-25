
library(shiny)
library(shinyWidgets)
library(tidyverse)


# run data from notebook


# Define UI for application that draws a histogram
ui <- fluidPage(


  # Application title
titlePanel("Demographic data"),

 fluidRow(
   #column(3,
    #      radioButtons("tot_avg_input",
     #                  "Select Total Stays or Average Stay Length",
      #                 choices = list("Total Stays" = tot_and_avg_stays$tot_stays,
       #                               "Average Length of Stay" = tot_stays_age$avg_stays),
        #               selected = 1),
         # ),
   
   column(3,
   pickerInput("age_input", 
               "Select Age Groups", 
               choices = unique(tot_stays_age_no_location$age),
               multiple =  TRUE,
               selected = unique(tot_stays_age_no_location$age)),
   ),
   
   column(6,
   pickerInput("quarter_input", 
               "Select Quarter", 
               choices = unique(tot_stays_age_no_location$quarter),
               multiple =  TRUE,
               selected = unique(tot_stays_age_no_location$quarter))
        )
   ),
 
 
          plotOutput("age_plot"),
 
 
         dataTableOutput("table_output")
    
 )   

 

# Define server logic required to draw a histogram

server <- function(input, output) {

output$table_output <- renderDataTable({
  
  dashboard_data_table %>% 
    filter(Age %in% input$age_input)%>% 
    filter(Quarter %in% input$quarter_input) 
})


  output$age_plot <- renderPlot({
    tot_stays_age_no_location %>%
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>%
        ggplot() +
          geom_col(position = "dodge", colour = "white", aes(x = age, 
                                           y = tot_stays, 
                                           fill = quarter)
                    ) +
          ggtitle("Total Stays per Age") +
          labs(x = "Age", y = "Total Stays") +
          scale_fill_manual(values = c("2020Q1" = "#061a1f",
                                       "2020Q2" = "#062e3c",
                                       "2020Q3" = "#074859",
                                       "2020Q4" = "#11667f",
                                       "2021Q1" = "#008b87",
                                       "2021Q2" = "#47899b",
                                       "2021Q3" = "#659799")
                            ) +
          facet_wrap(~sex) +
          theme_bw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
