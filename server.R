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

dtData = data.frame(merged_data)

names(Taipei) <- names(dtData)
names(N_Taipei) <- names(dtData)
names(Taoyuan) <- names(dtData)
names(Taichung) <- names(dtData)
names(Tainan) <- names(dtData)
names(Kaohsiung) <- names(dtData)

options(scipen = 999)




shinyServer (
  function (input,output,session){

	output$city <- renderUI({
	  selectInput("City", "Please Select An Employee Number:",
				  choices = c('台北市','新北市', '桃園市', '台中市',
							  '台南市', '高雄市',
							  '基隆市', '新竹市', '新竹縣', '苗栗縣',
							  '彰化縣', '南投縣', '雲林縣', '嘉義市',
							  '嘉義縣', '屏東縣', '宜蘭縣', '花蓮縣',
							  '台東縣', '澎湖縣', '金門縣', '連江縣'
							  ))
	})

	output$linetype <- renderUI({
	  selectInput("Linetype", "Please Select An Employee Number:",
				  choices = c('食品餐飲類', '衣著飾品類',	'旅館住宿類',
							  '交通類',	'文教康樂類',		'百貨類', '其他類'))
	})

	output$educate <- renderUI({
	  selectInput("Educate", "Please Select An Employee Number:",
				  choices = c('博士.', '碩士.', '大學.', '專科.',
							  '高中高職.', '其他.'))
	})

	output$type <- renderUI({
	  selectInput("Type", "Please Select An Employee Number:",
				  choices = c('筆數.', '金額.新台幣.'))



	})
	
	
	


	output$myPlot <- renderPlot({

	  cp = toString(paste(paste(input$Linetype, input$Educate, sep = ''), input$Type, sep = ''))
	  seData  <- dtData[dtData$'地區' == input$City, cp]
	  Et  <- ts(seData, start=1,end=61)
	  plot(seData,  col="blue", type="l")

	  

	})
	
	output$fullPlot <- renderPlot({
	  cp = toString(paste(paste(input$Linetype, input$Educate, sep = ''), input$Type, sep = ''))
	  
	  dgData = data.frame(
	    Time = 1:61,
	    Signal =
	      c(
	        as.numeric(as.character(unlist(N_Taipei[dtData$'地區' == input$City, cp]))),
	        as.numeric(as.character(unlist(Taoyuan[dtData$'地區' == input$City, cp]))),
	        as.numeric(as.character(unlist(Taichung[dtData$'地區' == input$City, cp]))),
	        as.numeric(as.character(unlist(Tainan[dtData$'地區' == input$City, cp]))),
	        as.numeric(as.character(unlist(Kaohsiung[dtData$'地區' == input$City, cp])))
	        
	      ),
	    
	    VariableLabel = c('臺北市','新北市','桃園市','台中市', 
	                      '台南市','高雄市')
	  )
	  # base plot
	  p2 = ggplot(dgData, aes(x = Time, y = Signal,
	                          group = VariableLabel, fill = VariableLabel)) +stat_steamgraph()
	  
	  
	  # Area plot
	  p2 +
	    xlab('時間') +
	    ylab('各縣市')
	  
	})


  })


#View(dtData[dtData$VariableLabel == '台北市', ])
#ggg = toString(paste('食品餐飲類博士.', '筆數.', sep = ''))

#View(dtData[dtData$'地區' == '新北市', ggg] )
#print(names(dtData))
#View(dtData)
