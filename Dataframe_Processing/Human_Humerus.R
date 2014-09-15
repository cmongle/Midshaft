#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


setwd("//Users/Carrie/Desktop/")
humerusdataframe <- read.table('Ape.Midshaft_Slice.Geometry.Ouput_humerus.txt', header=T, sep="\t")


#Now we want only the "human" data
#use grep function to pull out labels with Pan in beginning of character string
human.rows <- grep("Human_", as.matrix(humerusdataframe$label))

#Additional code for humans to include Point Hope
PH.rows <- grep("^PH_", as.matrix(humerusdataframe$label))
combined <- c(human.rows, PH.rows)

#now pull out only those row levels which included human
human.humerusdataframe <- humerusdataframe[combined,]


#Create rows for J and Imin/Imax
human.humerusdataframe$J <- human.humerusdataframe$Imin + human.humerusdataframe$Imax
human.humerusdataframe$ratio <- human.humerusdataframe$Imax / human.humerusdataframe$Imin


#dataframe must be attached for this function to work
data <- human.humerusdataframe
attach(data)




