#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Call libraries
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)


# Load data
kpi_beds <- read_csv("kpi_beds_clean.csv") %>%
  mutate(across(is.numeric, round, digits=1))  

kpi_diff_beds <- read_csv("kpi_diff_beds_clean.csv") %>%
  mutate(across(is.numeric, round, digits=1)) 

# Get drop-down list menu options (and delete any occurring NAs)
hb_name_labels <- unique(kpi_beds$HBName) %>% 
  discard(is.na)


kpi_labels <- c("All Staffed Beds", 
                "Total Occupied Beds", 
                "Average Available Staffed Beds", 
                "Average Occupied Beds", 
                "Percentage Occupancy")

# The palette as per SF:
phs_palette <- c("#99DAF5", "#004785", "#C027B9", "#82BB25", "red", "yellow")



# Define UI for application that draws trend plots
ui <- fluidPage(

    # Application title
    h1(strong("Acute care vs. NHS bed numbers"), align="center", style = "font-size:100px;"),
    
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

# Define server logic required to draw trend plots
server <- function(input, output) {

    output$trend_plot <- renderPlot({

      kpi_beds %>%
        filter(HBName == input$nhs_board_input) %>%
        filter(Location == HB) %>%
        ggplot() +
        aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, fill = HBName) +
        geom_col(colour = "black") +
        geom_text(aes(label = .data[[input$kpi_input]]), vjust = -0.5) +
        scale_fill_manual(guide = "none", values = phs_palette[1]) +
        ylim(c(0, NA))
        
    })
    
    output$diff_plot <- renderPlot({
      
      kpi_diff_beds %>%
        filter(HBName == input$nhs_board_input) %>%
        filter(Location == HB) %>%
        ggplot() +
        aes(x = Quarter, y = .data[[input$kpi_input]], group = HBName, fill = HBName) +
        geom_col(colour = "black") +
        ylab("Pre-Covid19 Percentage Difference (%)") +
        geom_text(aes(label = .data[[input$kpi_input]]), vjust = -0.5) +
        scale_fill_manual(guide = "none", values = phs_palette[1])
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


