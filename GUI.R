# Load necessary libraries
library(shiny)
library(readxl)
library(randomForest)
library(gbm)
library(DALEX)
library(DALEXtra)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

# Load the dataset
university_students_data <- read.csv("cleaned_university_Data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("College Students' Spending Habits in Urban Saudi Arabia"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Add filters and input elements here
      selectInput("gender", "Select Gender:", c("All", unique(university_students_data$Gender))),
      sliderInput("age", "Select Age Range:", min = 18, max = 35, value = c(18, 35)),
      checkboxInput("part_time_job", "Part-time Job", FALSE),
      selectInput("study_year", "Select Study Year:", c("All", unique(university_students_data$Study_year))),
      selectInput("living", "Select Living Situation:", c("All", unique(university_students_data$Living))),
      checkboxInput("scholarship", "Scholarship", FALSE),
      selectInput("transporting", "Select Transporting:", c("All", unique(university_students_data$Transporting))),
      checkboxInput("smoking", "Smoking", FALSE),
      checkboxInput("coffee_or_energy_drinks", "Coffee or Energy Drinks", FALSE),
      selectInput("games_and_hobbies", "Select Games and Hobbies:", c("All", unique(university_students_data$Games_and_Hobbies))),
      selectInput("cosmetics_and_selfcare", "Select Cosmetics and Selfcare:", c("All", unique(university_students_data$Cosmetics_and_Selfcare))),
      hr(style = "border-color:#ec6607"),
      tags$div(
        title = "Download all the data",
        actionButton(
          inputId = 'download_data',
          label = "Download Data",
          icon = icon("download"),
          style = "color: #fff; background-color: #ec6607; border-color: #ec6607"
        )
      )
    ),
    
    mainPanel(
      # Add output elements (plots, tables, etc.) here
      tabsetPanel(
        tabPanel("Visualization", plotOutput("expenses_plot")),
        tabPanel("Data Table", dataTableOutput("data_table"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Filter the dataset based on user input
  filtered_data <- reactive({
    data <- university_students_data
    
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender, ]
    }
    
    data <- data[data$Age >= input$age[1] & data$Age <= input$age[2], ]
    
    if (input$part_time_job) {
      data <- data[data$Part_time_job == "Yes", ]
    }
    
    if (input$study_year != "All") {
      data <- data[data$Study_year == input$study_year, ]
    }
    
    if (input$living != "All") {
      data <- data[data$Living == input$living, ]
    }
    
    if (input$scholarship) {
      data <- data[data$Scholarship == "Yes", ]
    }
    
    if (input$transporting != "All") {
      data <- data[data$Transporting == input$transporting, ]
    }
    
    if (input$smoking) {
      data <- data[data$Smoking == "Yes", ]
    }
    
    if (input$coffee_or_energy_drinks) {
      data <- data[data$Coffee_or_Energy_Drinks == "Yes", ]
    }
    
    if (input$games_and_hobbies != "All") {
      data <- data[data$Games_and_Hobbies == input$games_and_hobbies, ]
    }
    
    if (input$cosmetics_and_selfcare != "All") {
      data <- data[data$Cosmetics_and_Selfcare == input$cosmetics_and_selfcare, ]
    }
    
    return(data)
  })
  
  
  
  
  # Create an expenses plot
  output$expenses_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Age, y = Monthly_expenses, color = Gender)) +
      geom_point() +
      labs(title = "Monthly Expenses vs. Age", x = "Age", y = "Monthly Expenses")
  })
  
  # Create a data table
  output$data_table <- renderDataTable({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
