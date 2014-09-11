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

##########################################################################################
#CSA Analysis ####
# Function to pull out csa at intervals and compile into dataframe 

#CURRENTLY WORKING AND SAME AS SLADEK
#all functions need to be changed to reflect this!####
midshaft.csa.function <- function(x)
{
  slicelength <- as.numeric(length(csa[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slicelength <- slicelength + (min(slice[which(label == x)])-1)
  slice.bind <- cbind(seq((min(slice[which(label == x)])), slicelength), (csa[which(label == x)]))
  slice.bind <- rbind(matrix(nrow = (min(slice[which(label == x)])-1), ncol=2), slice.bind)
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  csa.levels <- data.frame(rbind(slice.matrix[,2]))
  csa.levels <- cbind(x, csa.levels)
  colnames(csa.levels) <- c("specimen", seq(0,95, by=5))
  perdiff <- rbind(((csa.levels[,2:21] - csa.levels[,12])/csa.levels[,12])*100)/length(unique(x))
  perdiff <- data.frame(cbind(x, perdiff))
  colnames(perdiff) <- c("specimen",seq(0,95, by=5))
  return(perdiff)
}

x <- as.vector(data$label)

result.csa = mdply(unique(x), midshaft.csa.function)


#Reshape into long format in order to plot 
result.csa.long <- melt(result.csa, variable.name="level", value.name="percent.difference")

result.csa.long <- rbind(result.csa.long[which(result.csa.long$level == 30),], 
                         result.csa.long[which(result.csa.long$level == 35),],
                         result.csa.long[which(result.csa.long$level == 40),],
                         result.csa.long[which(result.csa.long$level == 45),],
                         result.csa.long[which(result.csa.long$level == 50),],
                         result.csa.long[which(result.csa.long$level == 55),],
                         result.csa.long[which(result.csa.long$level == 60),],
                         result.csa.long[which(result.csa.long$level == 65),],
                         result.csa.long[which(result.csa.long$level == 70),])



#Now the same function, but with the raw data in order to perform the statistical tests (this is just repeating all steps up to, but not including calculating percentages) 
midshaft.raw.csa.function <- function(x)
{
  slicelength <- as.numeric(length(csa[which(label == x)]))
  slice.intervals <- as.integer(seq(1,slicelength, by=(slicelength*.05)))
  slice.intervals <- slice.intervals + min(slice[which(label == x)])
  slicelength <- slicelength + (min(slice[which(label == x)])-1)
  slice.bind <- cbind(seq((min(slice[which(label == x)])), slicelength), (csa[which(label == x)]))
  slice.bind <- rbind(matrix(nrow = (min(slice[which(label == x)])-1), ncol=2), slice.bind)
  slice.matrix <- as.matrix(slice.bind[slice.intervals, ])
  csa.levels <- data.frame(rbind(slice.matrix[,2]))
  csa.levels <- cbind(x, csa.levels)
  colnames(csa.levels) <- c("specimen", seq(0,95, by=5))
  return(csa.levels)
}


raw.csa.result = mdply(unique(x), midshaft.raw.csa.function)
raw.result.csa.long <- melt(raw.csa.result, variable.name="level", value.name="raw.value")


#inverse of previous result.csa.long to keep specimen names for the anova-- probably need to change for all 
raw.result.csa.long <- raw.result.csa.long[-c((which(raw.result.csa.long$level == 0)), (which(raw.result.csa.long$level == 5)),(which(raw.result.csa.long$level == 10)), (which(raw.result.csa.long$level == 15)),(which(raw.result.csa.long$level == 20)), (which(raw.result.csa.long$level == 25)),(which(raw.result.csa.long$level == 75)),(which(raw.result.csa.long$level == 80)),(which(raw.result.csa.long$level == 85)),(which(raw.result.csa.long$level == 90)),(which(raw.result.csa.long$level == 95))),]


#Repeated measures ANOVA####
#Standard repeated measures anova (correcting for repeated measures from the same specimen)
m <- aov(terms(raw.result.csa.long$raw.value ~ raw.result.csa.long$level + (raw.result.csa.long$specimen)))
m <- TukeyHSD(m)
midshaft.aov <- c('50-30','50-35','50-40','50-45', '55-50', '60-50', '65-50','70-50')
csa.aov <- m$`raw.result.csa.long$level`[midshaft.aov,]



#Paired T-test of all levels against midshaft
csa.ttest <-as.data.frame(cbind( c(30,35,40,45,55,60, 65,70), c(t.test(raw.csa.result$'50', raw.csa.result$'30', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'35', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'40', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'45', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'55', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'60', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'65', paired=T)$p.value,
                                                                t.test(raw.csa.result$'50', raw.csa.result$'70', paired=T)$p.value)))
colnames(csa.ttest) <- c("level", "p-value")
#Bonferroni Correction to these p-values
csa.ttest[,2] <- p.adjust(csa.ttest[,2], method = "bonferroni", n = length(csa.ttest[,2]))

#Compare Results of ANOVA and T-test:
csa.pvalues <- cbind(csa.aov[,-c(1:3)], csa.ttest[,2])
colnames(csa.pvalues) <- c("R.M. ANOVA", "Paired T-Test")
csa.pvalues


#And the same significance tests on the logged values####
m <- aov(terms(log(raw.result.csa.long$raw.value) ~ raw.result.csa.long$level + (raw.result.csa.long$specimen)))
m <- TukeyHSD(m)
midshaft.aov <- c('50-30','50-35','50-40','50-45', '55-50', '60-50', '65-50','70-50')
log.csa.aov <- m$`raw.result.csa.long$level`[midshaft.aov,]

#Paired T-test of all levels against midshaft
log.csa.ttest <-as.data.frame(cbind( c(30,35,40,45,55,60, 65,70), c(t.test(log(log(raw.csa.result$'50')), log(raw.csa.result$'30'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'35'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'40'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'45'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'55'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'60'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'65'), paired=T)$p.value,
                                                                t.test(log(raw.csa.result$'50'), log(raw.csa.result$'70'), paired=T)$p.value)))
colnames(log.csa.ttest) <- c("level", "p-value")
#Bonferroni Correction to these p-values
log.csa.ttest[,2] <- p.adjust(csa.ttest[,2], method = "bonferroni", n = length(csa.ttest[,2]))


#Compare Results of logged ANOVA and T-test:
log.csa.pvalues <- cbind(log.csa.aov[,-c(1:3)], log.csa.ttest[,2])
colnames(log.csa.pvalues) <- c("R.M. ANOVA", "Paired T-Test")
log.csa.pvalues


