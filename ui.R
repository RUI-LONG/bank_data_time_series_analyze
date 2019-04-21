shinyUI(
  pageWithSidebar (
    headerPanel ("My Shiny App"),

    sidebarPanel (
      uiOutput("city"),
      uiOutput("linetype"),
      uiOutput("type"),
      uiOutput("educate")
      

    ),
    mainPanel(plotOutput("myPlot"),plotOutput("fullPlot"))
    

  )
)
