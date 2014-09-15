#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


setwd("//Users/Carrie/Desktop/")
radiusdataframe <- read.table('Ape.Midshaft_Slice.Geometry.Output_radius.txt', header=T, sep="\t")


#Now we want only the "human" data
#use grep function to pull out labels with Pan in beginning of character string
human.rows <- grep("Human_", as.matrix(radiusdataframe$label))

#Additional code for humans to include Point Hope
PH.rows <- grep("^PH_", as.matrix(radiusdataframe$label))
combined <- c(human.rows, PH.rows)

#now pull out only those row levels which included human
human.radiusdataframe <- radiusdataframe[combined,]


#Create rows for J and Imin/Imax
human.radiusdataframe$J <- human.radiusdataframe$Imin + human.radiusdataframe$Imax
human.radiusdataframe$ratio <- human.radiusdataframe$Imax / human.radiusdataframe$Imin


#dataframe must be attached for this function to work
data <- human.radiusdataframe
attach(data)




