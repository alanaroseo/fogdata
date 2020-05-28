library(dplyr)
light <- as.data.frame(read.delim("clipboard"))
names(light)

TT <- as.data.frame(read.delim("clipboard"))

TTcv <- as.data.frame(read.delim("clipboard"))
names(TT)

heightbin <- as.factor(light$heightbin)

heightbin <- as.factor(TT$heightbin)

lightvar <- light%>%
  group_by(heightbin)%>%
  dplyr::summarize(Variance=var(DSF),Mean=mean(DSF),SD=sd(DSF))

lightvar <- as.data.frame(lightvar)
lightvar <- as.data.frame(read.delim("clipboard"))
names(lightvar)

#############
cv <- ggplot(data=TTcv, aes(y= CV,x=heightbin))+
  geom_point(aes( fill =heightbin), alpha = .7,show.legend = FALSE, size = 6, color ="darkgreen", shape = 21, stroke=2)+
  labs(y = "",
       x = "") +
  xlim(40,110)+
  scale_fill_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()


cv+theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color="black", size =1))

ph = 5
pw = 4.5
ggsave("TTCVplot.tiff", height=ph, width=pw)


###########
lightplot <- ggplot(data=lightvar, aes(y= DSF,x=height))+
  geom_point(aes( fill =height,shape = type),  size = 6, color="darkgreen",stroke=1.5,show.legend = FALSE)+
  stat_smooth(aes(color=type),method="loess", se=F, size = 1.5)+
  scale_color_manual(values = c( "darkgreen","chartreuse2" ))+
  scale_shape_manual(values=c(21,2))+
  labs(y = "",
       x = "") +
  xlim(40,110)+
  scale_fill_gradient(TTvar, low = "chartreuse2", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()


lightplot+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_line(color="black", size =1))

ph = 5
pw = 4.5
ggsave("lightplot.tiff", height=ph, width=pw)
#######
TTvar <- as.data.frame(read.delim("clipboard"))

TTvar <- TT%>%
  group_by(heightbin)%>%
  dplyr::summarize(Variance=var(TTv_per_m2),Mean=mean(TTv_per_m2),SD=sd(TTv_per_m2))

names(TTvar)

TTvar$CV <- (TTvar$SD/TTvar$Mean)
######

Tvar <- ggplot(data=TTvar, aes(y= value,x=heightbin))+
  geom_point(aes( fill =heightbin),  size = 5, shape = 21,color="darkgreen",stroke=1.5,show.legend = FALSE)+
  stat_smooth(aes(color=type),method="loess", se=F, size = 1.5)+
  scale_color_manual(values = c( "darkgreen","chartreuse2" ))+
  labs(y = "",
       x = "") +
  xlim(50,110)+
  scale_fill_gradient(TTvar, low = "chartreuse2", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()

