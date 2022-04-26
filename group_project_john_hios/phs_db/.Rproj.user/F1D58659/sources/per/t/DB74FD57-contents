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
beds <- read_csv("beds_clean.csv") 


# Get drop-down list menu options
hb_name_labels <- unique(beds$hb_name) %>% 
  discard(is.na)

shb_name_labels <- unique(beds$shb_name)  %>% 
  discard(is.na)

country_name_labels <- unique(beds$country_name) %>% 
  discard(is.na)

kpi_labels <- c("total_occupied_beds", "average_available_staffed_beds",
               "average_occupied_beds", "percentage_occupancy")


# The palette as per SF:
phs_palette <- c("#99DAF5", "#004785", "#C027B9", "#82BB25")



# Define UI for application that draws trend plots
ui <- fluidPage(

    # Application title
    titlePanel("Acute care vs. NHS bed numbers"),
    
    # ADD theme here
    # theme = shinytheme("united"),

    # Sidebar with a drop-down menu 
    sidebarLayout(
      sidebarPanel(
        selectInput("nhs_board_input",
                    "Select Board",
                    choices = hb_name_labels
        ),
        
        selectInput("kpi_input",
                    "Select Key Performance Index (KPI)",
                    choices = kpi_labels
        )
        
        
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("trend_plot")
      )
    )

)

# Define server logic required to draw trend plots
server <- function(input, output) {

    output$trend_plot <- renderPlot({

      beds %>%
        filter(hb_name == input$nhs_board_input) %>%
        filter(location == hb) %>%
        ggplot() +
        aes(x = quarter, y = .data[[input$kpi_input]], group = country_name, colour = hb_name) +
        # aes_string(x = "quarter", y = .data[["input$kpi_labels"]], group = "country_name", colour = "hb_name") +
        geom_line() +
        geom_point() +
        scale_colour_manual(guide = "none", values = phs_palette[2]) +
        ylim(c(0, NA))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


