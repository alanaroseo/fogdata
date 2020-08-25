library(ggplot2)
library(dplyr)


psy <- as.data.frame(read.delim("clipboard"))

names(psy)
psy$Date <- as.Date(psy$Date)

ggplot(data = psy, aes(x = Date, y = humidity)) +
  geom_path(color = "orchid", size=2,alpha = .7)+
  theme_classic()
  
stat_smooth(method="lm", se=F, aes(color = value),size = 1.5,show.legend = FALSE)+
  
  coeff <- 100

g <- ggplot(data=psy, aes(x=Date)) +
  
  geom_path( aes(y=delta_MPA, color = value )) +
  geom_path(aes(y=humidity/coeff),color = "orchid", size=2,alpha = .7) +
  theme_classic()
    
    # Add a second axis and specify its features
g+  sec.axis = sec_axis(humidity*coeff, name="Second Axis")


ggplot(data = psy, aes(x = max_humidity.bin, y = delta_MPA)) +
  geom_boxplot(aes(color = min_humidity.bin), size=1,alpha = .7)+
  geom_abline(slope = 0,intercept = 0, size =1, color = "midnightblue", alpha =.4)+
  theme_classic()

