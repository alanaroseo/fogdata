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

S <- ggplot(data = squish, aes(x = condition, y = Transfusion.Tissue.Area..mm2.)) + #####use this one squish#####
geom_boxplot(aes(fill=condition),alpha = .7,show.legend = FALSE, size = 1.5)+
  labs(y = expression(paste("Transfusion", " ", "Tissue", " " , "Area", " " ,paste((mm^2),")", sep = "")), size=10),
       x = "") +
  scale_fill_manual(values = c( "seagreen", "mediumaquamarine","darkgreen"))+
  theme_classic( base_size = 30)

S <- S+geom_point(color="purple4", alpha=.75, size=6)
  
  
S <- S + xlim("Hydrated","Droughted","Fog exposed")
S+theme(axis.text.x = element_text(color="black"),
         axis.text.y = element_text( color="black"))

ph = 4
pw = 14
ggsave("squish.tiff", height=ph, width=pw)

#############

V <- ggplot(data = dots, aes(x = position, y = max.VPD.diff)) + #####use this one VPD#####
geom_boxplot(aes(fill=position),alpha = .7,show.legend = FALSE, size = 1.5)+
  labs(y = expression(paste("Maximum" ,"  ", italic(Delta),"VPD","  ", "(kPa)", sep = ""), size=10),
       x = "") +
  scale_fill_manual(values = c("mediumaquamarine" ,"seagreen","darkgreen" ))+
  theme_classic( base_size = 30 )+
  coord_flip()

V<- V+geom_point(color="purple4", alpha=.75, size=6)
V+theme(axis.text.x = element_text( color="black"),
        axis.text.y = element_text( color="black"))

ph = 14
pw = 5.5
ggsave("VPDplot.tiff", height=ph, width=pw)

######
 
  
  
G <- ggplot(data = Gs, aes(x = position, y = gs)) + #####use this one gs#####
geom_boxplot(aes(fill=position),alpha = .7,show.legend = FALSE, size =1.5)+
  labs(y = expression(paste(paste(g[Smax])," ", 
                            "(mol ",paste(m^-2 )," ",  paste( sec^-1),")" ), size=10),
       x = "") +
  scale_fill_manual(values = c("mediumaquamarine" ,"seagreen","darkgreen" ))+
  theme_classic( base_size = 30 )+
  coord_flip()

G <- G+geom_point(color="purple4", alpha=.75, size=6)
G+theme(axis.text.x = element_text( color="black"),
        axis.text.y = element_text(color="black"))

ph = 14
pw = 5
ggsave("gsplot.tiff", height=ph, width=pw)



ggplot(data = Vdiff, aes(x = position, y = max.VPD.diff)) +
  geom_boxplot(aes(fill=position), alpha = .7)+
  theme_classic()+
  facet_wrap(~Site)
############ 
