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


# Load data
beds <- <- read_csv("beds_clean.csv.csv") 
# all_months <- unique(temp_sco$month)



# Define UI for application that draws trend plots
ui <- fluidPage(

    # Application title
    titlePanel("Acute care vs. NHS bed numbers"),
    
    # ADD theme here
    # theme = shinytheme("united"),

    # Sidebar with a drop-down menu 
    sidebarLayout(
      sidebarPanel(
        "Sidebar",
        "Some other text in the sidebar"
        selectInput("nhs_board_input",
                    "Select Board",
                    choices = all_months
        )
        
        
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        "Main section",
        "Some other text in the main section"
        # plotOutput("distPlot")
      )
    )

)

# Define server logic required to draw trend plots
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
