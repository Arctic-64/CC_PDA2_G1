
library(shiny)
library(shinyWidgets)
library(tidyverse)


# run data from notebook


# Define UI for application that draws a histogram
ui <- fluidPage(


  # Application title
titlePanel("Demographic data"),

 fluidRow(
   column(3,
   pickerInput("age_input", 
               "Select Age Groups", 
               choices = unique(tot_stays_age$age),
               multiple =  TRUE,
               selected = unique(tot_stays_age$age)),
   ),
   
   column(6,
   pickerInput("quarter_input", 
               "Select Quarter", 
               choices = unique(tot_stays_age$quarter),
               multiple =  TRUE,
               selected = unique(tot_stays_age$quarter))
        )
   ),
 
 
          plotOutput("age_plot"),
 
 
         dataTableOutput("table_output")
    
 )   

 

# Define server logic required to draw a histogram

server <- function(input, output) {

output$table_output <- renderDataTable({
  
  tot_stays_age %>% 
    filter(age %in% input$age_input)%>% 
    filter(quarter %in% input$quarter_input) 
})


  output$age_plot <- renderPlot({
    tot_stays_age %>% 
      filter(age %in% input$age_input) %>% 
      filter(quarter %in% input$quarter_input) %>% 
        ggplot() +
          geom_col(position = "dodge", aes(x = age, 
                                           y = tot_stays, 
                                           fill = quarter)
                    ) +
          ggtitle("Total Stays per Age") +
          labs(x = "Age", y = "Total Stays") +
          scale_fill_manual(values = c("2020Q1" = "#004785",
                                       "2020Q2" = "#99DAF5",
                                       "2020Q3" = "#004796",
                                       "2020Q4" = "#99DAF9",
                                       "2021Q1" = "#004707",
                                       "2021Q2" = "#99DAF0",
                                       "2021Q3" = "#004718")) +
          facet_wrap(~sex) +
          theme_bw()
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
