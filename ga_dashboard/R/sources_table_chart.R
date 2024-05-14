library(shiny)
library(shinydashboard)

ui <- fluidPage(
  dashboardHeader(title = "Table and Chart"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table", tabName = "table"),
      menuItem("Chart", tabName = "chart")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table", dataTableOutput("table")),
      tabItem(tabName = "chart",
              fluidRow(
                column(6, plotOutput("chart")),
                column(6, actionButton("view_chart", "View Chart"))
              )
      )
    )
  )
)

server <- function(input, output) {
  output$table <- renderDataTable({
    data.frame(x = 1:10, y = 1:10)
  })
  
  output$chart <- renderPlot({
    ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_line()
  })
  
  observeEvent(input$view_chart, {
    output$chart <- renderPlot({
      ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_line()
    })
  })
}

shinyApp(ui, server)
