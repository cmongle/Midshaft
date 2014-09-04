#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all humerus data
setwd("//Users/Carrie/Desktop/")
humerusdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_humerus.txt", header=T, sep="\t")


#Now we want only the chimpanzee data
#use grep function to pull out labels with Pan in beginning of character string
pan.rows <- grep("^Pan_", as.matrix(humerusdataframe$label))
#now pull out only those row levels which included pan
pan.humerusdataframe <- humerusdataframe[pan.rows,]

pan.humerusdataframe$J <- pan.humerusdataframe$Imin + pan.humerusdataframe$Imax
pan.humerusdataframe$ratio <- pan.humerusdataframe$Imax / pan.humerusdataframe$Imin


#dataframe must be attached for this function to work
data <- (pan.humerusdataframe)
attach(data)