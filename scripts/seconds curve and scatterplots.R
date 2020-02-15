TT <-  as.data.frame(read.delim("clipboard"))
names(TT)

tt <-  as.data.frame(read.delim("clipboard"))
names(tt)



#
p <- ggplot(data = TT, aes(x = height, y = seconds))+
  geom_point(color="darkgreen", alpha=.5, size=6,show.legend = FALSE)+
  theme_classic( base_size = 30 )+
  coord_flip()

p <- p+stat_smooth(method="lm", 
              formula=(y ~ poly(x,2, raw = TRUE)), se=F, color="purple4",size = 1.5)




ph = 12
pw = 5.5
ggsave("seconds.tiff", height=ph, width=pw)
#####


ggplot(data = tt, aes(x = height, y = Volume/1000))+
  geom_point(aes(color=Transfusion.tissue.volume.per.m.2), alpha=.5, size=6,show.legend = FALSE)+
  stat_smooth(method="loess", se=F, aes(color=Transfusion.tissue.volume.per.m.2),size = 1.5,show.legend = FALSE)+
  scale_color_manual(values = c( "darkgreen","mediumaquamarine"))+
  theme_classic( base_size = 30 )
 


ph = 8
pw = 7
ggsave("TTplot.tiff", height=ph, width=pw)


colors()
require(MASS)
show.colors(type=c("singles", "shades", "gray"), order.cols=TRUE)
demo(colors())


plotCol(nearRcolor("orchid", "rgb", dist=50), nrow=3)




