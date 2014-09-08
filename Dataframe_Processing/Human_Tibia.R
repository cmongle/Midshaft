#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all tibia data
setwd("//Users/Carrie/Desktop/")
tibiadataframe <- read.table("Ape.Midshaft_Slice.Geometry.Ouput_tibia.txt", header=T, sep="\t")

#Now we want only the "Human" data
#use grep function to pull out labels with Human in beginning of character string
Human.rows <- grep("^Human_", as.matrix(tibiadataframe$label))

#Additional code for humans to include Point Hope (currently not working for tibia)
#PH.rows <- grep("^PointHope_", as.matrix(tibiadataframe$label))
#combined <- c(Human.rows, PH.rows)

#now pull out only those row levels which included Human
Human.tibiadataframe <- tibiadataframe[PH.rows,]


#Create rows for J and Imin/Imax
Human.tibiadataframe$J <- Human.tibiadataframe$Imin + Human.tibiadataframe$Imax
Human.tibiadataframe$ratio <- Human.tibiadataframe$Imax / Human.tibiadataframe$Imin


#dataframe must be attached for this function to work
data <- Human.tibiadataframe
attach(data)
