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



merged_data = rbind(data_mct, data_loc)

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
names(Taipei) <- NULL
names(N_Taipei) <- NULL
names(Taoyuan) <- NULL
names(Taichung) <- NULL
names(Tainan) <- NULL
names(Kaohsiung) <- NULL

# Visualization
# creating some data

# convert from exponential notation to normal notation
options(scipen = 999)

dfData = data.frame(x = 1:61,
                    y = Taipei[1:61,3])



p1 = ggplot_waterfall(
   dtData = dfData,

   'x',
   'y'
)


p1 +
  xlab('時間') +
  ylab('筆數')


# creating some data

dtData = data.frame(
   Time = 1:61,
   Signal =
      c(
         as.numeric(as.character(unlist(Taipei[1:61,4]))),
         as.numeric(as.character(unlist(N_Taipei[1:61,4]))),
         as.numeric(as.character(unlist(Taoyuan[1:61,4]))),
         as.numeric(as.character(unlist(Taichung[1:61,4]))),
         as.numeric(as.character(unlist(Tainan[1:61,4]))),
         as.numeric(as.character(unlist(Kaohsiung[1:61,4])))

      ),

   VariableLabel = c(rep('臺北市', 61),
                     rep('新北市', 61), rep('桃園市', 61),
                     rep('台中市', 61), rep('台南市', 61),
                     rep('高雄市', 61))
)

# base plot
p2 = ggplot(dtData, aes(x = Time, y = Signal,
                        group = VariableLabel, fill = VariableLabel)) +stat_steamgraph()


# Area plot
p2 +
   xlab('時間') +
   ylab('各縣市')



runApp("E:/bank_data_time_series_analyze")
