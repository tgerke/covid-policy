source(here::here("R", "plot.R"))
library(shiny)

actions <- colnames(dat)[2:13]

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("action", "Action", actions)
    ),
    mainPanel(
      plotOutput("timeline", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  output$timeline <- renderPlot({
    plot_action_timeline(dat, !!rlang::sym(input$action))
  })
}

shinyApp(ui, server)
