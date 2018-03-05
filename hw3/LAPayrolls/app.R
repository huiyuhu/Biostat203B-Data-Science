#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# Import the RDS datasets
totpay <- readRDS("totpay.rds")
highp <- readRDS("highpay.rds")
meanp <- readRDS("meanpay.rds")
medp <- readRDS("medpay.rds")
cost <- readRDS("cost.rds")
plan <- readRDS("plan.rds")

# Define tab-based UI by using navbarPage
ui <- navbarPage( 
  theme = shinytheme("united"), 
  "LA City Employee Payroll",
  tabPanel("Total payroll by LA City",
           titlePanel("Total payroll by LA City"),
           plotOutput(outputId = "Totpay_plot")
           ),
  
  # For question 1
  tabPanel(
    "Who earned most?",
    titlePanel("Who earned most?"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          "rows",
          "Choose number of row:",
          10,
          min = 0,
          max = 20
        ),
        numericInput("year", "Choose a year:",
                     2017, min = 2013, max = 2017)
      ),
      
      mainPanel(tableOutput(outputId = "high_pay"))
    )
  ),
  
  # For question 2
  tabPanel(
    "Which departments earn most?",
    titlePanel("Which departments earn most?"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          "rows1",
          "Choose number of row:",
          5,
          min = 0,
          max = 20
        ),
        numericInput("year1", "Choose a year:",
                     2017, min = 2013, max = 2017),
        radioButtons("method", "Choose a method:",
                     c("Mean", "Median"), "Median")
      ),
      
      mainPanel(tableOutput(outputId = "mpay"))
    )
  ),
  
  # For question 3
  tabPanel(
    "Which departments cost most?",
    titlePanel("Which departments cost most?"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          "rows2",
          "Choose number of row:",
          5,
          min = 0,
          max = 20
        ),
        numericInput("year2", "Choose a year:", 2017, min = 2013, max = 2017)
      ),
      
      mainPanel(tableOutput(outputId = "high_cost"))
    )
  ),
  
  # For question 4
  tabPanel(
    "Benefits Plan",
    titlePanel("Benefits Plan for each year"),
    sidebarLayout(sidebarPanel(
      numericInput("year3", "Choose a year:", 2017, min = 2013, max = 2017)
    ),
    
    mainPanel(plotOutput(outputId = "plan_plot"))
    )
  )
)

server <- function(input, output) {
  # For question 1
  output$Totpay_plot <- renderPlot({
    ggplot(data = totpay, aes(x = Year, y = payment, fill = class)) +
      geom_bar(stat = "identity")
  })
  
  # For question 2
  output$high_pay <- renderTable({
    highp %>% filter(Year == input$year) %>%
      head(input$rows)
  })
  
  #Which departments earn most
  output$mpay <- renderTable({
    if (input$method == "Mean") {
      meanp %>% filter(Year == input$year1) %>%
        head(input$rows1)
    } else {
      medp  %>% filter(Year == input$year1) %>%
        head(input$rows1)
    }
  })
  
  ##Which departments cost most?
  output$high_cost <- renderTable({
    cost %>% filter(Year == input$year2) %>%
      head(input$rows2)
  })
  
  ## plan
  output$plan_plot <- renderPlot({
    plan %>%
      filter(Year == input$year3) %>%
      ggplot(aes(x = "", y = n, fill = `Benefits Plan`)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y")
  })
}

shinyApp(ui = ui, server = server)