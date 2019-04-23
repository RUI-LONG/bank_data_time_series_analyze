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
library(vioplot)
library(prettydoc)

#setwd('./bank_data_time_series_analyze')
data_loc <- read_excel("./BANK_LOC_ALL_EL.xlsx")
data_mct <- read_excel("./BANK_MCT_ALL_EL.xlsx")

#if(!exists("foo", mode="function")) source("ggplot_waterfall.R")
#if(!exists("foo", mode="function")) source("stat_steamgraph.R")
### change "107å¹?12???" to "107-12"

AD_convert <- function(date){
  date <- gsub("???","",gsub("å¹?","/",date))


  return(date)
}

Taipei_convert <- function(city){
  city <- gsub("?°??—å??","Taipei",city)


  return(city)
}

merged_data = rbind(data_mct, data_loc)

#merged_data[,2] <- apply(merged_data[,2],1,FUN=Taipei_convert)

Taipei <- merged_data[grep("?°??—å??", merged_data$?œ°??€), ]
N_Taipei <- merged_data[grep("?–°??—å??", merged_data$?œ°??€), ]
Taoyuan <- merged_data[grep("æ¡ƒå?’å??", merged_data$?œ°??€), ]
Taichung <- merged_data[grep("?°ä¸­å??", merged_data$?œ°??€), ]
Tainan <- merged_data[grep("?°??—å??", merged_data$?œ°??€), ]
Kaohsiung <- merged_data[grep("é«˜é?„å??", merged_data$?œ°??€), ]

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
	  selectInput("City", "ä¾ç…§ç¸?å¸‚å?†æ??:",
				  choices = c('?°??—å??','?–°??—å??', 'æ¡ƒå?’å??', '?°ä¸­å??',
							  '?°??—å??', 'é«˜é?„å??',
							  '?Ÿº??†å??', '?–°ç«¹å??', '?–°ç«¹ç¸£', '??—æ?—ç¸£',
							  'å½°å?–ç¸£', '??—æ?•ç¸£', '?›²??—ç¸£', '??‰ç¾©å¸?',
							  '??‰ç¾©ç¸?', 'å±æ±ç¸?', 'å®œè˜­ç¸?', '?Š±?“®ç¸?',
							  '?°?±ç¸?', 'æ¾Žæ?–ç¸£', '??‘é?€ç¸?', '?€?æ±Ÿç¸£'
							  ))
	})

	output$linetype <- renderUI({
	  selectInput("Linetype", "ä¾ç…§å­¸æ­·é¡žåˆ¥??†æ??:",
				  choices = c('é£Ÿå?é?é£²é¡?', 'è¡???—é£¾??é??',	'??…é¤¨ä½å®¿é¡?',
							  'äº¤é€šé??',	'??‡æ?™åº·æ¨‚é??',		'?™¾è²¨é??', '?…¶ä»–é??'))
	})

	output$educate <- renderUI({
	  selectInput("Educate", "ä¾ç…§å­¸æ­·??†æ??:",
				  choices = c('??šå£«.', 'ç¢©å£«.', 'å¤§å­¸.', 'å°ˆç??.',
							  'é«˜ä¸­é«˜è·.', '?…¶ä»?.'))
	})

	output$type <- renderUI({
	  selectInput("Type", "ä¾ç…§æ¶ˆè²»??‡æ?™å?†æ??:",
				  choices = c('ç­†æ•¸.', '??‘é??.?–°?°å¹?.'))



	})
	
	
	


	output$myPlot <- renderPlot({

	  cp = toString(paste(paste(input$Linetype, input$Educate, sep = ''), input$Type, sep = ''))
	  new_cp = toString(paste(input$City, cp, sep = ''))
	  seData  <- dtData[dtData$'?œ°??€' == input$City, cp]
	  Et  <- ts(seData, start=1,end=61)
	  plot(seData,  col="blue", type="l",  main="??˜ç?šå??",
	       xlab=new_cp)

	  

	})
	
	output$fullPlot <- renderPlot({
	  
	  cp = toString(paste(paste(input$Linetype, input$Educate, sep = ''), input$Type, sep = ''))
	  new_cp = toString(paste(input$City, cp, sep = ''))
	  boxplot(dtData[dtData$'?œ°??€' == input$City, cp],data=dtData, main="?›´?–¹???", 
	          xlab=new_cp)
	  
	})


  })


#View(dtData[dtData$VariableLabel == '?°??—å??', ])
#ggg = toString(paste('é£Ÿå?é?é£²é¡žå?šå£«.', 'ç­†æ•¸.', sep = ''))

#View(dtData[dtData$'?œ°??€' == '?–°??—å??', ggg] )
#print(names(dtData))
#View(dtData)
#x1 <- dtData[dtData$'?œ°??€' == '?–°??—å??', ggg]

#vioplot(x1,  names=c("4 cyl"), col="gold")


# upload to server
#library(rsconnect)

#rsconnect::deployApp('E:/bank_data_time_series_analyze')

