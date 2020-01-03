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


ggplot(data = TT, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = TSF, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = d13C, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = TSF, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = d13C, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE)+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TT, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=tree, shape = position), alpha = .7, size = 3)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()

ggplot(data = TT, aes(x = TSF, y = ind.seconds.open )) +
  geom_point(aes(color=tree, shape = position), alpha = .7, size = 3)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

ggplot(data = TT, aes(x = d13C, y = ind.seconds.open )) +
  geom_point(aes(color=tree, shape = position), alpha = .7, size = 3)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()


 #all data
######
#height-paired data
######


ggplot(data = TTp, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = TSF, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = d13C, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()+
  facet_wrap(~tree)


ggplot(data = TTp, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

ggplot(data = TTp, aes(x = TSF, y = ind.seconds.open )) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()

ggplot(data = TTp, aes(x = d13C, y = ind.seconds.open )) +
  geom_point(aes(color=position, shape = tree), alpha = .7)+
  geom_smooth(method=lm, se=FALSE, aes(color = position))+
  theme_classic()



ggplot(data = TTp, aes(x = mol_rel.m2, y = ind_pred_gs)) +
  geom_point(aes(color=TSF, size = height, shape=position), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")+
  facet_wrap(~tree)

ggplot(data = TTp, aes(x = vol_rel, y = xy_vol)) +
  geom_point(aes(color=position, size = height, shape=tree), alpha = .7)+
  theme_classic()+
  geom_abline(size=1, color = "blue")


  
ggplot(data = TTp, aes(x =  ind_VPD, y = ind_pred_gs)) +
  geom_point(aes(color=tree, size = DSF, shape=position), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")

ggplot(data = TTp, aes(x =  ind_VPD, y = mol_rel.m2)) +
  geom_point(aes(color=tree, size = DSF, shape=position), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")

ggplot(data = TTp, aes(x = height, y = ind.seconds.open)) +
  geom_point(aes(color=tree,shape=position, size = TT_volume.mm3.), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")


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


ggplot(data = TTp, aes(x = DSF, y = TTv_per_m2)) +
  geom_point(aes(color=ind_pred_gs,shape=position, size = height), alpha = .7)+
  theme_classic()+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "salmon")
  



ggplot(data = TTp, aes(x= position, y = ind.seconds.open)) +
  geom_boxplot(aes(color=tree))+
  theme_classic()
 #height-paired samples only