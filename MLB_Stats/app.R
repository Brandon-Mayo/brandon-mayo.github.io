#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(plotly)

mlb23 <- read.csv("mlb23.csv")

#-------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(
    title="MLB Scoring Data"),
  "This application will create a graph from any two variables choosen, and show the value of r^2. 
  Along with this you can also choose to show the line of best fit, and the error bands. This data used is from the 2023
  season and was taken from 'Baseball Reference'.",
  sidebarLayout(
    sidebarPanel( 
      selectInput("stat1", label="Variable 1 (x-axis)", choices=names(mlb23)[-1], selected="runs"),
      
      selectInput("stat2", label="Variable 2 (y-axis)", choices=names(mlb23)[-1],selected="bat_avg"),
      
      checkboxInput("line", label="Line of best fit and R^2"),
      
      conditionalPanel(
        condition = "input.line == true",
        checkboxInput("error", label="Error bands"))
      ),
    
    
    mainPanel(
      plotOutput("plot", brush="plot_brush"),
      textOutput("r_squared"),
      textOutput("equation"),
      tableOutput("info")
    )
  )
)

#-------------------------------------------------------------------------------

server <- function(input, output) {
    p <- reactive({
      ggplot(mlb23, aes_string(x=input$stat1, y=input$stat2)) +
        geom_point() 
  })
    
    output$plot <- renderPlot({
      if(input$line == F){
        p()
      }else{
        p()+geom_smooth(method="lm", se=ifelse(input$error, TRUE, FALSE))
      }
    })
    
    output$info <- renderTable({
      brushedPoints(mlb23, input$plot_brush)
    })
    
    output$r_squared <- renderText({
      if(input$line == T){
        model <- lm(as.formula(paste(input$stat2, "~", input$stat1)), data=mlb23)
        r_squared <- summary(model)$r.squared
        paste("R^2:", round(r_squared, 3))
      } else{
        return()
      }
    })
    
    output$equation <- renderText({
      if(input$line == T){
        model <-lm(as.formula(paste(input$stat2, "~", input$stat1)), data=mlb23)
        equation <- paste("Equation:", round(coef(model)[1], 3), "*x + ", round(coef(model)[2], 3))
      } else{
        return()
      }
    })
  }

#-------------------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)






