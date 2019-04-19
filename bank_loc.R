library(readxl)
library(stringr)
library(shiny)
library(ggplot2)
library(data.table)

setwd('E:/bank_data_time_series_analyze')
data_loc <- read_excel("BANK_LOC_ALL_EL.xlsx")
data_mct <- read_excel("BANK_MCT_ALL_EL.xlsx")

if(!exists("foo", mode="function")) source("ggplot_waterfall.R")

## Check data dimension
#dim(data_mct)
#dim(data_loc)
#dim(merged_data)

#View(merged_data)
merged_data = rbind(data_mct, data_loc)

Taipei <- merged_data[grep("台北市", merged_data$地區), ]
#View(Taipei)

# 62 2
#merged_data[1:62,3]


# Visualization 
# creating some data
set.seed(1)
dfData = data.frame(x = 1:61,
                    y = Taipei[1:61,3], 
                    inherit.aes = FALSE )

# base plot
p1 = ggplot_waterfall(
   dtData = Taipei,
   '地區',
   '食品餐飲類博士[筆數]'
)

# adding some formatting

p1 +
  xlab(NULL) +
  ylab(NULL)
