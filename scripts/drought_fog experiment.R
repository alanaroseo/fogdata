TT <- as.data.frame(read.delim("clipboard"))

TTp <- as.data.frame(read.delim("clipboard"))

library(plyr)
library(tidyverse)
library(ggplot2)
library("viridis")
library(plotly)
names(TTp)
names(TT)

TT$gs <- TT$predicted.gs.from.DeWalt.curve..mol.H2O.m.2s.1.

TT$tree <- as.factor(TT$tree)
TT$position <- as.factor(TT$position)
##
TTp$gs <- TTp$predicted.gs.from.DeWalt.curve..mol.H2O.m.2s.1.

TTp$tree <- as.factor(TTp$tree)
TTp$position <- as.factor(TTp$position)
TTp$position <- revalue(TTp$position, c("1"="inner", "2"="outer"))


TT$position <- revalue(TT$position, c("1"="inner", "2"="outer"))



#all data
#####
ggplot(data = TT, aes(x = height, y = seconds.open )) +
  geom_point(aes(color=fraction.TT.vol.released, shape = tree), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~position)

ggplot(data = TT, aes(x = height, y = seconds.open )) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

plot.new()
ggplot(data = TT, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_abline(size=1, color = "blue")

ggplot(data = TTp, aes(x =new_ind_gs , y =mol_rel.m2) ) +
  geom_point(aes(shape=position, color = height, size = DSF), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  scale_color_viridis()+
  facet_wrap(~tree)
  
ggplot(data = TT, aes(x = height, y = seconds.open)) +
  geom_point(aes(color=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")
 

ggplot(data = TT, aes(x = height, y = mol_rel.m2)) +
  geom_point(aes(color=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  stat_smooth(method='nls', formula='y~(-c*x^2)+b*x-a', method.args=list(start = 
                                                                  list(a=1, b=1, c=.001)), se=FALSE)

ggplot(data = TT, aes(x = TTv_per_m2, y = new_ind_gs)) +
  geom_point(aes(color=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  stat_smooth(method='nls', formula='y~(-c*x^2)+b*x-a', method.args=list(start = 
                                                                           list(a=1, b=1, c=.001)), se=FALSE)
 

ggplot(data = TT, aes(x= position, y = xy_vol.vol_rel)) +
  geom_boxplot(aes(color=position))+
  theme_classic()



ggplot(data = TT, aes(x= position, y = ind_sec_open)) +
  geom_boxplot(aes(color=position))+
  theme_classic()
  

ggplot(data = TT, aes(x = height, y = ind_sec_open )) +
  geom_point(aes(color=fraction.TT.vol.released, shape = tree), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~position)

ggplot(data = TT, aes(x = height, y = ind_sec_open )) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

plot.new()
ggplot(data = TT, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_abline(size=1, color = "blue")

ggplot(data = TT, aes(x = mol_rel.m2, y = ind_pred_gs)) +
  geom_point(aes(color=position, size = height), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = height, y = ind_sec_open)) +
  geom_point(aes(color=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")


ggplot(data = TT, aes(x = ind_pred_gs, y = mol_rel.m2)) +
  geom_point(aes(color=position, size = TT_volume.mm3., alpha=height))+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  facet_wrap(~tree)


ggplot(data = TT, aes(x= position, y = xy_vol.vol_rel)) +
  geom_boxplot(aes(color=position))+
  theme_classic()



ggplot(data = TT, aes(x= position, y = seconds.open)) +
  geom_boxplot(aes(color=position))+
  theme_classic()

 #all data
######
#height-paired data
######
ggplot(data = TTp, aes(x = height, y = new5_sec_open )) +
  geom_point(aes(color=fraction.TT.vol.released, shape = tree), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~position)

ggplot(data = TTp, aes(x = height, y = new5_sec_open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)


ggplot(data = TTp, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_abline(size=1, color = "blue")

ggplotly(ggplot(data = TTp, aes(x = mol_rel.m2, y = All_gs_nomidstrat)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon"))

 ggplot(data = TTp, aes(x = height, y = seconds.open) )+
  geom_point(aes(color=position, size = TT_volume.mm3., shape=tree), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")


  
ggplot(data = TTp, aes(x = gs, y = mol_rel.m2)) +
  geom_point(aes(color=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")


ggplot(data = TTp, aes(x= position, y = xy_vol.vol_rel)) +
  geom_boxplot(aes(color=position))+
  theme_classic()



ggplot(data = TTp, aes(x= position, y = ind_sec_open)) +
  geom_boxplot(aes(color=position))+
  theme_classic()


ggplot(data = TTp, aes(x = height, y = ind_sec_open )) +
  geom_point(aes(color=fraction.TT.vol.released, shape = position), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = height, y = ind_sec_open )) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

plot.new()
ggplot(data = TTp, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_abline(slope=1, intercept=0)

ggplot(data = TTp, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "purple")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = mol_rel.m2, y = ind_pred_gs)) +
  geom_point(aes(color=TSF, size = height, shape=position), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = TT..tot.A, y = ind_pred_gs)) +
  geom_point(aes(color=mol_rel.m2, size = height, shape=position), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = TT..tot.A, y = ind_pred_gs)) +
  geom_point(aes(color=tree, size = DSF, shape=position), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")
  
ggplot(data = TTp, aes(x =  TTv_per_m2, y = ind_pred_gs)) +
  geom_point(aes(color=tree, size = DSF, shape=position), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")


ggplot(data = TTp, aes(x = height, y = ind_sec_open)) +
  geom_point(aes(color=tree,shape=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")

ggplot(data = TTp, aes(x = height, y = ind_sec_open)) +
  geom_point(aes(color=DSF,shape=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = height, y = TTv_per_m2)) +
  geom_point(aes(color=ind_pred_gs,shape=position, size = DSF), alpha = .7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")

ggplot(data = TTp, aes(x = ind_pred_gs, y = TTv_per_m2)) +
  geom_point(aes(color=height,shape=position, size = DSF), alpha = .7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = ind_pred_gs, y = mol_rel.m2)) +
  geom_point(aes(color=tree,shape=position, size = DSF), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")

ggplot(data = TTp, aes(x = height, y = ind_sec_open)) +
  geom_point(aes(color=ind_pred_gs,shape=position, size = DSF), alpha = .7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)
  
ggplot(data = TTp, aes(x = DSF, y = TTv_per_m2)) +
  geom_point(aes(color=ind_pred_gs,shape=position, size = height), alpha = .7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")
  
ggplot(data = TTp, aes(x = xy_vol, y = vol_rel)) +
  geom_point(aes(color=ind_pred_gs, size = height, shape=position),alpha=.7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  geom_abline()

ggplot(data = TTp, aes(x = xy_vol, y = vol_rel)) +
  geom_point(aes(color=ind_pred_gs, size = height),alpha=.7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  geom_abline()+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = height, y = ind_pred_gs)) +
  geom_point(aes( color = TT_volume.mm3., size=mol_rel.m2),alpha=.7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = TT_volume.mm3., y = mol_rel.m2)) +
  geom_point(aes(color=height, shape = tree),alpha=.7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")

ggplot(data = TTp, aes(x =height , y = TTv_per_m2)) +
  geom_point(aes(color=tree, shape = position),alpha=.7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")

ggplot(data = TTp, aes(x = height, y = mol_rel.m2)) +
  geom_point(aes(color=tree, size = TTv_per_m2, shape=position),alpha=.7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x= position, y = xy_vol.vol_rel)) +
  geom_boxplot(aes(color=position))+
  theme_classic()

ggplot(data = TTp, aes(x= position, y = xy_vol.vol_rel)) +
  geom_boxplot(aes(color=position))+
  theme_classic()



ggplot(data = TTp, aes(x= position, y = ind_sec_open)) +
  geom_boxplot(aes(color=tree))+
  theme_classic()
 #height-paired samples only