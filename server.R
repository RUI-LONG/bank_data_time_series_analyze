library(xts)
library(Rcpp)
library(shiny)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(TSstudio)
library(data.table)

setwd('E:/bank_data_time_series_analyze')
data_loc <- read_excel("BANK_LOC_ALL_EL.xlsx")
data_mct <- read_excel("BANK_MCT_ALL_EL.xlsx")

if(!exists("foo", mode="function")) source("E:/bank_data_time_series_analyze/ggTimeSeries-master/R/ggplot_waterfall.R")
if(!exists("foo", mode="function")) source("E:/bank_data_time_series_analyze/ggTimeSeries-master/R/stat_steamgraph.R")
### change "107年12月" to "107-12"

AD_convert <- function(date){
  date <- gsub("月","",gsub("年","/",date))
  
  
  return(date)
}

Taipei_convert <- function(city){
  city <- gsub("台北市","Taipei",city)
  
  
  return(city)
}

merged_data = rbind(data_mct, data_loc)

#merged_data[,2] <- apply(merged_data[,2],1,FUN=Taipei_convert)

Taipei <- merged_data[grep("台北市", merged_data$地區), ]
N_Taipei <- merged_data[grep("新北市", merged_data$地區), ]
Taoyuan <- merged_data[grep("桃園市", merged_data$地區), ]
Taichung <- merged_data[grep("台中市", merged_data$地區), ]
Tainan <- merged_data[grep("台南市", merged_data$地區), ]
Kaohsiung <- merged_data[grep("高雄市", merged_data$地區), ]




Taipei[,1] <- apply(Taipei[,1],1,FUN=AD_convert)
N_Taipei[,1] <- apply(N_Taipei[,1],1,FUN=AD_convert)
Taoyuan[,1] <- apply(Taoyuan[,1],1,FUN=AD_convert)
Taichung[,1] <- apply(Taichung[,1],1,FUN=AD_convert)
Tainan[,1] <- apply(Tainan[,1],1,FUN=AD_convert)
Kaohsiung[,1] <- apply(Kaohsiung[,1],1,FUN=AD_convert)

# Convert title to null
#names(Taipei) <- c(1:length(Taipei))
names(Taipei) <- NULL
names(N_Taipei) <- NULL
names(Taoyuan) <- NULL
names(Taichung) <- NULL
names(Tainan) <- NULL
names(Kaohsiung) <- NULL




dtData = data.frame(
  Time = 1:61,
  Signal =
    c(
      as.numeric(as.character(unlist(Taipei[1:61,3:86]))),
      as.numeric(as.character(unlist(N_Taipei[1:61,3:86]))),
      as.numeric(as.character(unlist(Taoyuan[1:61,3:86]))),
      as.numeric(as.character(unlist(Taichung[1:61,3:86]))),
      as.numeric(as.character(unlist(Tainan[1:61,3:86]))),
      as.numeric(as.character(unlist(Kaohsiung[1:61,3:86])))
      
    ),
  
  VariableLabel = c(rep('台北市', 61),
                    rep('新北市', 61), rep('桃園市', 61),
                    rep('台中市', 61), rep('台南市', 61),
                    rep('高雄市', 61))
)

dtData <- cbind(dtData, merged_data[1:61,3:86])

shinyServer (
  function (input,output,session){
    
    output$employee <- renderUI({
      selectInput("Employee", "Please Select An Employee Number:",
                  choices = c('台北市','新北市', '桃園市', '台中市', 
                              '台南市', '高雄市'))
    })
    
    output$linetype <- renderUI({
      selectInput("Linetype", "Please Select An Employee Number:",
                  choices = c('食品餐飲類博士', '2', '3'))
    })
    
    output$type <- renderUI({
      selectInput("Type", "Please Select An Employee Number:",
                  choices = c('[筆數]', '[金額，新台幣]'))
    })
    
    output$symbol <- renderUI({
      if (!is.null(input$Employee)){
        selectInput ("Symbol", "Please Select Salary Symbol:",
                     choices = sort(unique(data[data$EmpNum == input$Employee, ]$Sym)))  
      }
    })
    
    
    output$myPlot <- renderPlot({
      
        
        seData  <- dtData[dtData$VariableLabel == input$Employee, 2]
        Et  <- ts(seData, start=1,end=61)
        plot(seData, col="blue", type="l")
        
   
     
    })
    
    
  })


#View(dtData[dtData$VariableLabel == '台北市', ])
ggg = toString(paste('食品餐飲類博士', '[筆數]', sep = ''))

View(dtData[dtData$VariableLabel == '台北市',] $ '食品餐飲類博士[筆數]')


