# Q1 Read the data into R
install.packages("readxl")
library(readxl)

my_data <- read_excel("/Users/zhengxiao/Documents/TextBooks/2017-S1/FIT5147-Data_exploration_and_visualisation/Assignment/Assignment_R/assignment-02-data.xlsx")
str(my_data)

# Q2 Using ggplot2 to show fro each kind of coral and for each site how the bleaching
# varies from year to year
# facet showing the bleaching for one kind of coral at one site across the time period
# The sites should be ordered by latitude
# Fit smoothers to the data
install.packages("ggplot2")
library(ggplot2)

coral <- ggplot(my_data, aes(x = year, y = bleaching)) + geom_jitter()
coral <- coral + facet_grid(latitude + site ~ kind) +  stat_smooth(method = "lm", se = FALSE, col = "red") 
coral

# Q3 Using shiny to create an interactive visualisation.
# Allowing the user to vary the kind of coral display and the choice of smoother
# Read data from xlsx dataset
install.packages("readxl")
library(readxl)
my_data = read_excel("/Users/zhengxiao/Documents/TextBooks/2017-S1/FIT5147-Data_exploration_and_visualisation/Assignment/Assignment_R/assignment-02-data.xlsx")

install.packages("ggplot2")
library(ggplot2)
install.packages("shiny")
library(shiny)


ui <-fluidPage(
  titlePanel("Question 3 - Interactive visualisation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select",
                  label = "Select a type of coral",
                  choices = list("blue corals", "hard corals", "sea fans", "sea pens", "soft corals"),
                  selected = "blue corals"),
      
      checkboxInput("smoother",
                    label = strong("smooth the data visualisation"),
                    value = FALSE)
    ),
    
    mainPanel(
      textOutput("text"),
      plotOutput("data1")
    )
  )
)

server <- function(input, output){
   output$text <- renderText({
    paste("Coral of ", input$select)
  })

   newData <- reactive({
     my_data[ which(my_data$kind == input$select), ]
   })
   
   output$data1 <- renderPlot({
     p <- ggplot(newData(), aes(x = year, y = bleaching)) + geom_jitter() + facet_grid(site + latitude ~ kind)
   
   if(input$smoother){
     p <- p + stat_smooth(method = "lm", se = FALSE, col = "red")
   }
     print(p)
  })
}

shinyApp(ui = ui, server = server)
