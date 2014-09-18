#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all tibia data
setwd("//Users/Carrie/Desktop/")
tibiadataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_tibia.txt", header=T, sep="\t")

#Now we want only the "pan" data
#use grep function to pull out labels with pan in beginning of character string
pan.rows <- grep("^Pan_", as.matrix(tibiadataframe$label))


#now pull out only those row levels which included pan
pan.tibiadataframe <- tibiadataframe[pan.rows,]


#Create rows for J and Imin/Imax
pan.tibiadataframe$J <- pan.tibiadataframe$Imin + pan.tibiadataframe$Imax
pan.tibiadataframe$ratio <- pan.tibiadataframe$Imax / pan.tibiadataframe$Imin


#dataframe must be attached for this function to work
data <- pan.tibiadataframe
attach(data)

