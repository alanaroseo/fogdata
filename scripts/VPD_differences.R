Vdiff <-  as.data.frame(read.delim("clipboard"))
names(Vdiff)

Gs <-  as.data.frame(read.delim("clipboard"))
names(Gs)

dots <-  as.data.frame(read.delim("clipboard"))
names(dots)

squish <-  as.data.frame(read.delim("clipboard"))
names(squish)

library(ggplot2)
library(ggsci)
max(seconds_t_max)
##########
squish$height <- as.factor(squish$height)

S <- ggplot(data = squish, aes(y = condition, x = Transfusion.Tissue.Area..mm2.)) + #####use this one squish#####
  geom_point(aes(fill=condition, shape = height) ,  stroke = 1,alpha = .75,show.legend = FALSE, size =10, color ="white") +
  labs(y = "",
       x = "") +
  xlim(0,0.015)+
  ylim("Fogged","Droughted","Hydrated")+
  scale_fill_manual(values = c( "yellow3","seagreen4", "midnightblue"))+
  scale_shape_manual(values = c(21:24))+
  theme_classic( base_size = 30)
  


S+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_line(color="black", size =1))


ph = 7
pw = 7
ggsave("squish.tiff", height=ph, width=pw)
######without axes:

S+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_blank(),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_blank())

#############

V <- ggplot(data = Vdiff, aes(x = Height, y = max.VPD.diff)) + #####use this one VPD#####
geom_point(aes( fill =Height), alpha = .7,show.legend = FALSE, size = 6, color ="darkgreen", shape = 21, stroke=2)+
  stat_smooth(method="lm", se=F, color = "darkgreen",size = 1.5,show.legend = FALSE)+
  labs(y = "",
       x = "") +
  scale_fill_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()

V <- V+scale_y_discrete(limits=seq(.75,1.75))


V+theme(axis.text.y = element_text(color="black"),
             axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
             axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
             axis.ticks.x = element_line(color="black", size =1))

ph = 5
pw = 4.5
ggsave("VPDplot.tiff", height=ph, width=pw)

######

  
G <- ggplot(data = Gs, aes(x = height, y = max.gs)) + #####use this one gs#####
geom_point(aes( fill =height), alpha = .7,show.legend = FALSE, size = 6, color ="darkgreen", shape = 21, stroke=2)+
  stat_smooth(method="lm", se=F, color = "darkgreen",size = 1.5,show.legend = FALSE)+
  labs(y = "",
       x = "") +
  scale_fill_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()


G+theme(axis.text.y = element_blank( ),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_blank( ),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_blank( ),
        axis.ticks.x = element_line(color="black", size =1))

ph = 5
pw = 4
ggsave("gsplot.tiff", height=ph, width=pw)



ggplot(data = Vdiff, aes(x = position, y = max.VPD.diff)) +
  geom_boxplot(aes(fill=position), alpha = .7)+
  theme_classic()+
  facet_wrap(~Site)
############ 

l <- ggplot(data = light, aes(x = height, y = DSF)) + #####use this one VPD#####
geom_point(aes( fill =height), alpha = .7,show.legend = FALSE, size = 6, color ="darkgreen", shape = 21, stroke=2)+
  stat_smooth(method="lm", se=F, color = "darkgreen",size = 1.5,show.legend = FALSE)+
  labs(y = "",
       x = "") +
  xlim(40,110)+
  ylim(0,100)+
  scale_fill_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()




l+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_line(color="black", size =1))

ph = 5
pw = 4.5
ggsave("lightplot.tiff", height=ph, width=pw)
