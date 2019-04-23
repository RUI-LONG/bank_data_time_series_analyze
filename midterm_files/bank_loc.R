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

#setwd('E:/bank_data_time_series_analyze')
data_loc <- read_excel("BANK_LOC_ALL_EL.xlsx")
data_mct <- read_excel("BANK_MCT_ALL_EL.xlsx")

if(!exists("foo", mode="function")) source("ggplot_waterfall.R")
if(!exists("foo", mode="function")) source("stat_steamgraph.R")
### change "107å¹?12???" to "107-12"

AD_convert <- function(date){
   date <- gsub("???","",gsub("å¹?","/",date))
   
   
   return(date)
}



merged_data = rbind(data_mct, data_loc)

Taipei <- merged_data[grep("?°??—å??", merged_data$?œ°??€), ]
N_Taipei <- merged_data[grep("?–°??—å??", merged_data$?œ°??€), ]
Taoyuan <- merged_data[grep("æ¡ƒå?’å??", merged_data$?œ°??€), ]
Taichung <- merged_data[grep("?°ä¸­å??", merged_data$?œ°??€), ]
Tainan <- merged_data[grep("?°??—å??", merged_data$?œ°??€), ]
Kaohsiung <- merged_data[grep("é«˜é?„å??", merged_data$?œ°??€), ]

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
  xlab('??‚é??') +
  ylab('ç­†æ•¸')


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

   VariableLabel = c(rep('?‡º??—å??', 61),
                     rep('?–°??—å??', 61), rep('æ¡ƒå?’å??', 61),
                     rep('?°ä¸­å??', 61), rep('?°??—å??', 61),
                     rep('é«˜é?„å??', 61))
)

# base plot
p2 = ggplot(dtData, aes(x = Time, y = Signal,
                        group = VariableLabel, fill = VariableLabel)) +stat_steamgraph()


# Area plot
p2 +
   xlab('??‚é??') +
   ylab('??„ç¸£å¸?')



#runApp("E:/bank_data_time_series_analyze")

#runGitHub( "bank_data_time_series_analyze", "RUI-LONG") 

