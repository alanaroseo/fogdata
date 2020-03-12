TT <-  as.data.frame(read.delim("clipboard"))
names(TT)

tt <-  as.data.frame(read.delim("clipboard"))
names(tt)

sec <- as.data.frame(read.delim("clipboard"))
names(sec)

#
p <- ggplot(data = TT, aes(x = height, y = seconds))+
  geom_jitter(aes( fill =height), alpha = .7,show.legend = FALSE, size = 10, shape = 21,color="darkgreen",stroke=2)+
  labs(y = "",
       x = "") +
  scale_fill_gradient(TT, low = "greenyellow", high = "darkgreen")+
  theme_classic(base_size = 30)+
  coord_flip()

p <- p+stat_smooth(method="lm", 
              formula=(y ~ poly(x,2, raw = TRUE)), se=F, color="purple4",size = 1.5)

p+theme(axis.text.y = element_blank( ),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_blank( ),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_blank( ),
        axis.ticks.x = element_line(color="black", size =1))





ph = 14
pw = 5.5
ggsave("seconds.tiff", height=ph, width=pw)
#####


ggplot(data = tt, aes(x = height, y = Volume/1000))+
  geom_point(aes(color=Transfusion.tissue.volume.per.m.2), alpha=.5, size=6,show.legend = FALSE)+
  stat_smooth(method="loess", se=F, aes(color=Transfusion.tissue.volume.per.m.2),size = 1.5,show.legend = FALSE)+
  scale_color_manual(values = c( "darkgreen", "greenyellow"))+
  theme_classic( base_size = 30 )+
  coord_flip()
 


ph = 8
pw = 7
ggsave("TTplot.tiff", height=ph, width=pw)


#####

S<- ggplot(data = sec, aes(x = height, y = seconds))+
  geom_point(aes(fill=senario, shape = position), alpha=.5, size=6, shape =21, color="purple4", show.legend = FALSE)+
  stat_smooth(method="loess", se=F, aes(color=senario),size = 1.5, show.legend = FALSE)+
  scale_fill_manual(values = c("darkgreen","midnightblue", "chartreuse2"))+
  scale_color_manual(values = c("darkgreen","midnightblue", "chartreuse2"))+
  theme_classic( base_size = 30 )+
  coord_flip()



S+theme(axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="black"),
        axis.line.y = element_line(color="black",size = 1.52),
        axis.line.x = element_line(color="black",size = 1.52),
        axis.ticks.y = element_line(color="black", size =1),
        axis.ticks.x = element_line(color="black", size =1))


ph = 14
pw = 8
ggsave("sec.tiff", height=ph, width=pw)

########

colors()
require(MASS)
show.colors(type=c("singles", "shades", "gray"), order.cols=TRUE)
demo(colors())


plotCol(nearRcolor("orchid", "rgb", dist=50), nrow=3)




