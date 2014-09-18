#Require packages:
library(plyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Load and attach data ####
#Load source file with all radius data
setwd("//Users/Carrie/Desktop/")
radiusdataframe <- read.table("Ape.Midshaft_Slice.Geometry.Output_radius.txt", header=T, sep="\t")


#Now we want only the chimpanzee data
#use grep function to pull out labels with Pan in beginning of character string
pan.rows <- grep("^Pan_", as.matrix(radiusdataframe$label))
#now pull out only those row levels which included pan
pan.radiusdataframe <- radiusdataframe[pan.rows,]

pan.radiusdataframe$J <- pan.radiusdataframe$Imin + pan.radiusdataframe$Imax
pan.radiusdataframe$ratio <- pan.radiusdataframe$Imax / pan.radiusdataframe$Imin


#dataframe must be attached for this function to work
data <- (pan.radiusdataframe)
attach(data)