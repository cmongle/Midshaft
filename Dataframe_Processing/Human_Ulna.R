#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


setwd("//Users/Carrie/Desktop/")
ulnadataframe <- read.table('Ape.Midshaft_Slice.Geometry.Ouput_ulna.txt', header=T, sep="\t")
str(ulnadataframe)

#Now we want only the "human" data
#use grep function to pull out labels with Pan in beginning of character string
human.rows <- grep("Human_", as.matrix(ulnadataframe$label))

#Additional code for humans to include Point Hope
PH.rows <- grep("^PH_", as.matrix(ulnadataframe$label))
combined <- c(human.rows, PH.rows)

#now pull out only those row levels which included human
human.ulnadataframe <- ulnadataframe[combined,]


#Create rows for J and Imin/Imax
human.ulnadataframe$J <- human.ulnadataframe$Imin + human.ulnadataframe$Imax
human.ulnadataframe$ratio <- human.ulnadataframe$Imax / human.ulnadataframe$Imin


#dataframe must be attached for this function to work
data <- Human.ulnadataframe
attach(data)




