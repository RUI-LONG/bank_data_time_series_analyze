shinyUI(
  pageWithSidebar (
    headerPanel ("My Shiny App"),

    sidebarPanel (
      uiOutput("city"),
      uiOutput("linetype"),
      uiOutput("type"),
      uiOutput("educate"),
      uiOutput("plottype")

    ),
    mainPanel(plotOutput("myPlot"))

  )
)
