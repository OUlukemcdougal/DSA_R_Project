#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#library
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Shiny App for my advanced r package"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("data_length", "Length of data (this will be the same for both x and y):", min = 1, max = 20, value = 30),
            numericInput("mean_x", "Enter a mean for the x:", min = 1, max = 20, value = 10),
            numericInput("mean_y", "Enter a mean for the y:", min = 1, max = 20, value = 8),
            numericInput("sd_x", "Enter a standard deviation for the x:", min = 1, max = 20, value = 15),
            numericInput("sd_y", "Enter a standard deviation for the y:", min = 1, max = 20, value = 15),
            numericInput("alpha", "Alpha:", min = 0, max = 1, value = 0.05),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot"),
          tableOutput("distPrint")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
      set.seed(32); x=rnorm(input$data_length, mean = input$mean_x, sd = input$sd_x)
      set.seed(35); y=rnorm(input$data_length, mean = input$mean_y, sd = input$sd_y)
      ans1 = ADVTTEST::myttest(x,y, alpha = input$alpha, paired = FALSE)
      plot(ans1)
    })
    output$distPrint <- renderTable({
      print(ans1)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
