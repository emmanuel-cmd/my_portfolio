library(shiny)
#library(shinydashboard)
library(rlang)
library(semantic.dashboard)
library(DT)
library(lintr)

ui <- dashboardPage(theme = "slate" , 
  dashboardHeader(title = "My dashboard"), 
  dashboardSidebar(sidebarMenu(menuItem("Iris", tabName = "iris", icon = icon("tree")),
                               menuItem("Cars",tabName = "cars", icon = icon("car")))), 
  dashboardBody(tabItems(tabItem("iris",
              box(plotOutput("correlation_plot"), width = 8),
              box(selectInput("features", "Features:", 
                              c("Sepal.Width", "Petal.Length", 
                                "Petal.Width")), width = 4)),
  tabItem("cars",fluidPage(h1("Cars"), dataTableOutput('carstable'))))) 
)

  

server <- function(input, output){
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length, iris[[input$features]], 
         xlab = "Sepal length", ylab = "Features")
})
  output$carstable <- renderDataTable(mtcars)
}

shinyApp(ui, server)

