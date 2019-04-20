shinyUI(
  pageWithSidebar (
    headerPanel ("<<統計資料分--期中報告>>"), 
    
    sidebarPanel (
      uiOutput("employee"),
      uiOutput("symbol"),
      uiOutput("linetype")
    ),
    mainPanel(plotOutput("myPlot"))
    
  )
)