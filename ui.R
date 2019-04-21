shinyUI(
  pageWithSidebar (
    headerPanel ("統計分析期中報告 - 消費額度分析"),

    sidebarPanel (
      uiOutput("city"),
      uiOutput("linetype"),
      uiOutput("type"),
      uiOutput("educate")
      

    ),
    mainPanel(plotOutput("myPlot"),plotOutput("fullPlot"))
    

  )
)
