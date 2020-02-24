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
##########
squish$height <- as.factor(squish$height)

S <- ggplot(data = squish, aes(x = condition, y = Transfusion.Tissue.Area..mm2.)) + #####use this one squish#####
geom_point(aes(color=condition, shape = height) ,  stroke = 8,alpha = .75,show.legend = FALSE, size =15) +
  labs(y = "",
       x = "") +
  scale_fill_manual(values = c( "yellow3","seagreen4", "midnightblue"))+
  scale_color_manual(values = c( "yellow3","seagreen4", "midnightblue"))+
  scale_shape_manual(values = c(0:2,5))+
  theme_classic( base_size = 30)

  
S <- S + xlim("Hydrated","Droughted","Fog exposed")

S <- S + ylim(0,0.015)

S+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_line(color="black", size =1))


ph = 5
pw = 13
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
geom_jitter(aes( color =Height), alpha = .7,show.legend = FALSE, size = 10, shape = 17)+
  labs(y = "",
       x = "") +
  scale_color_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()

V <- V+scale_y_discrete(limits=seq(0,2))


V+theme(axis.text.y = element_blank( ),
             axis.text.x = element_text(color="black"),
             axis.line.y = element_blank( ),
             axis.line.x = element_line(color="black",size = 1.52),
             axis.ticks.y = element_blank( ),
             axis.ticks.x = element_line(color="black", size =1))

ph = 14
pw = 4.5
ggsave("VPDplot.tiff", height=ph, width=pw)

######

  
G <- ggplot(data = Gs, aes(x = height, y = max.gs)) + #####use this one gs#####
geom_jitter(aes( color =height), alpha = .7,show.legend = FALSE, size = 10, shape = 17)+
  labs(y = "",
       x = "") +
  scale_color_gradient(Vdiff, low =  "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()


G+theme(axis.text.y = element_blank( ),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_blank( ),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_blank( ),
        axis.ticks.x = element_line(color="black", size =1))

ph = 14
pw = 4.5
ggsave("gsplot.tiff", height=ph, width=pw)



ggplot(data = Vdiff, aes(x = position, y = max.VPD.diff)) +
  geom_boxplot(aes(fill=position), alpha = .7)+
  theme_classic()+
  facet_wrap(~Site)
############ 
