####################################################################################################################################################################################

lo

##########################
#More simple plot that Ian preferred to look at the data ####

result.csa.long$percent.difference <- abs(result.csa.long$percent.difference)
ggplot(result.csa.long, aes(result.csa.long$level, result.csa.long$percent.difference, group=1)) +
stat_smooth() + geom_point()


ggplot(result.J.long, aes(result.J.long$level, result.J.long$percent.difference, group=1)) +
  geom_point() 

ggplot(result.ratio.long, aes(result.ratio.long$level, result.ratio.long$percent.difference, group=1)) +
  geom_point() 


#try to apply this function to calculate Standard error at each level since I am no longer using the anova around the percent difference
library(plotrix)
#needs to be mean percent difference + this value and then -this value


#Prepare the percent difference data for the plot (mean, std. error for percent differences at each level)
#This means result.csa.long no longer necessary

csa.std.e <- c(std.error(result.csa$'30'), 
           std.error(result.csa$'35'),
           std.error(result.csa$'40'), 
           std.error(result.csa$'45'), 
           std.error(result.csa$'50'),
           std.error(result.csa$'55'),
           std.error(result.csa$'60'),
           std.error(result.csa$'65'),
           std.error(result.csa$'70'))

csa.mean.perdiff <- c(mean(result.csa$'30'), 
                  mean(result.csa$'35'),
                  mean(result.csa$'40'), 
                  mean(result.csa$'45'), 
                  mean(result.csa$'50'),
                  mean(result.csa$'55'),
                  mean(result.csa$'60'),
                  mean(result.csa$'65'),
                  mean(result.csa$'70'))
csa.mean.perdiff <- abs(csa.mean.perdiff)                  
level <- c(30, 35, 40, 45, 50, 55, 60, 65, 70)
csa.percent.difference <- as.data.frame(cbind(level, csa.mean.perdiff, csa.std.e))



J.std.e <- c(std.error(result.J$'30'), 
             std.error(result.J$'35'),
             std.error(result.J$'40'), 
             std.error(result.J$'45'), 
             std.error(result.J$'50'),
             std.error(result.J$'55'),
             std.error(result.J$'60'),
             std.error(result.J$'65'),
             std.error(result.J$'70'))

J.mean.perdiff <- c(mean(result.J$'30'), 
                    mean(result.J$'35'),
                    mean(result.J$'40'), 
                    mean(result.J$'45'), 
                    mean(result.J$'50'),
                    mean(result.J$'55'),
                    mean(result.J$'60'),
                    mean(result.J$'65'),
                    mean(result.J$'70'))
J.mean.perdiff <- abs(J.mean.perdiff)                  
level <- c(30, 35, 40, 45, 50, 55, 60, 65, 70)
J.percent.difference <- as.data.frame(cbind(level, J.mean.perdiff, J.std.e))


ratio.std.e <- c(std.error(result.ratio$'30'), 
                 std.error(result.ratio$'35'),
                 std.error(result.ratio$'40'), 
                 std.error(result.ratio$'45'), 
                 std.error(result.ratio$'50'),
                 std.error(result.ratio$'55'),
                 std.error(result.ratio$'60'),
                 std.error(result.ratio$'65'),
                 std.error(result.ratio$'70'))

ratio.mean.perdiff <- c(mean(result.ratio$'30'), 
                        mean(result.ratio$'35'),
                        mean(result.ratio$'40'), 
                        mean(result.ratio$'45'), 
                        mean(result.ratio$'50'),
                        mean(result.ratio$'55'),
                        mean(result.ratio$'60'),
                        mean(result.ratio$'65'),
                        mean(result.ratio$'70'))
ratio.mean.perdiff <- abs(ratio.mean.perdiff)                  
level <- c(30, 35, 40, 45, 50, 55, 60, 65, 70)
ratio.percent.difference <- as.data.frame(cbind(level, ratio.mean.perdiff, ratio.std.e))




#Plotting the Absolute Mean Percent Differences +/- the standard error
pdf("Human Tibia.pdf", height=21, width=35)

result.plot <- ggplot(csa.percent.difference, aes(level, csa.mean.perdiff, group=1)) +
  scale_colour_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("blue", "orange", "dark red"))+
  scale_fill_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("light blue", "yellow", "red"))+
  geom_line(data=csa.percent.difference, aes(level, csa.mean.perdiff, colour="CSA", fill="CSA")) + 
  geom_ribbon(data=csa.percent.difference, aes(ymax=csa.percent.difference$csa.mean.perdiff + csa.percent.difference$csa.std.e, ymin=csa.percent.difference$csa.mean.perdiff - csa.percent.difference$csa.std.e, fill="CSA"), alpha=.2)+

  geom_line(data=J.percent.difference, aes(level, J.mean.perdiff, colour="J", fill="J")) + 
  geom_ribbon(data=J.percent.difference, aes(ymax=J.percent.difference$J.mean.perdiff + J.percent.difference$J.std.e, ymin=J.percent.difference$J.mean.perdiff - J.percent.difference$J.std.e, fill="J"), alpha=.2)+


  geom_line(data=ratio.percent.difference, aes(level,ratio.mean.perdiff, colour="Imax/Imin", fill="Imax/Imin")) + 
  geom_ribbon(data=ratio.percent.difference, aes(ymax=ratio.percent.difference$ratio.mean.perdiff + ratio.percent.difference$ratio.std.e, ymin=ratio.percent.difference$ratio.mean.perdiff - ratio.percent.difference$ratio.std.e, fill="Imax/Imin"), alpha=.2)

#Add Theme
result.plot + xlab('5% Intervals Along Diaphysis')+ ylab('Percent Difference from Midshaft') + theme_calc(base_size=28) + 
  theme(axis.title.x = element_text(vjust = -.4), axis.title.y = element_text(vjust = .7, angle = 90)) 

dev.off()



pdf("Human Tibia.pdf", height=21, width=35)

result.plot <- ggplot(csa.percent.difference, aes(level, csa.mean.perdiff, group=1)) +
  scale_colour_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("blue", "orange", "dark red"))+
  scale_fill_manual("", breaks=c("CSA","J","Imax/Imin"), values = c("light blue", "yellow", "red"))+
  geom_line(data=csa.percent.difference, aes(level, csa.mean.perdiff, colour="CSA", fill="CSA")) + 
  geom_ribbon(data=csa.percent.difference, stat='smooth',aes(ymax=csa.percent.difference$csa.mean.perdiff + csa.percent.difference$csa.std.e, ymin=csa.percent.difference$csa.mean.perdiff - csa.percent.difference$csa.std.e, fill="CSA"), alpha=.2)+
  
  geom_line(data=J.percent.difference,aes(level, J.mean.perdiff, colour="J", fill="J")) + 
  geom_ribbon(data=J.percent.difference, aes(ymax=J.percent.difference$J.mean.perdiff + J.percent.difference$J.std.e, ymin=J.percent.difference$J.mean.perdiff - J.percent.difference$J.std.e, fill="J", stat='smooth'), alpha=.2)+
  
  
  geom_line(data=ratio.percent.difference, aes(level,ratio.mean.perdiff, colour="Imax/Imin", fill="Imax/Imin")) + 
  geom_ribbon(data=ratio.percent.difference,aes(ymax=ratio.percent.difference$ratio.mean.perdiff + ratio.percent.difference$ratio.std.e, ymin=ratio.percent.difference$ratio.mean.perdiff - ratio.percent.difference$ratio.std.e, stat='smooth',fill="Imax/Imin"), alpha=.2)

#Add Theme
result.plot + xlab('5% Intervals Along Diaphysis')+ ylab('Percent Difference from Midshaft') + theme_calc(base_size=28) + 
  theme(axis.title.x = element_text(vjust = -.4), axis.title.y = element_text(vjust = .7, angle = 90)) 

dev.off()
