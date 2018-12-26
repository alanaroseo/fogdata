
fog <- as.data.frame(read.delim("clipboard"))

#fog by height graphical analysis

library(tidyverse)
library(ggplot2)
library("viridis")
# look at data
summary(fog)

ggplot(data = fog, aes(x = Minutes, y = weight_dif)) +
  geom_point(aes(color=Type))
  
ggplot(data = fog, aes(x = Minutes, y = weight_perA)) +
  geom_point(aes(color=Type))

ggplot(data = fog, aes(x = Minutes, y = MPa_perA )) +
  geom_point(aes(color=Type))

ggplot(data = fog, aes(x = Minutes, y = MPa_dif )) +
  geom_point(aes(color=tree, shape = Type, alpha = height)) 

ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=Type, shape = tree, alpha = height)) 

ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=height, shape = tree), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~Type)

ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=height, shape = Type), alpha = .7)+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=height, shape = tree, size = MPa_dif), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~Type)


ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=height, shape = Type, size = MPa_dif), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = weight_perA )) +
  geom_point(aes(color=MPa_dif, shape = Type), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = MPa_dif)) +
  geom_point(aes(color=height, shape = Type), alpha = .7)+
  scale_color_viridis()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = rel_w_dif)) +
  geom_point(aes(color=height, shape = tree), alpha = .7)+
  scale_color_viridis()+
  geom_smooth(method=lm, se=TRUE, color = "orchid")+
  theme_classic()+
  facet_wrap(~Type)

p <- ggplot(data = fog, aes(x = Minutes, y = rel_w_dif)) +
  geom_point(aes(color=height, shape = tree), alpha = .7)+
  scale_color_viridis()+
  stat_smooth(method="lm", se=TRUE, fill=NA,formula=y ~ poly(x, 3, raw=TRUE),colour="pink")+
  theme_classic()+
  facet_wrap(~Type)

p+annotate(geom = "text", x = 100, y = 10, label = my.eq, #my.eq is the name of the equation, p is name of plot
            family = "serif", hjust = 0, parse = TRUE, size = 4)
         

ggplot(data = fog, aes(x = Minutes, y = MPa_dif)) +
  geom_point(aes(color=height), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~Type)

ggplot(data = fog, aes(x = Minutes, y = weight_perA)) +
  geom_point(aes(color=height), alpha = .7)+
  scale_color_viridis()+
  theme_classic()+
  facet_wrap(~Type)

ggplot(data = fog, aes(x = Minutes, y = weight_perA)) +
  geom_jitter(aes(color=tree), alpha = .7)+
  theme_classic()+
  facet_wrap(~Type)

ggplot(data = fog, aes(x = Minutes, y = weight_perA)) +
  geom_jitter(aes(color=Type), alpha = .7,  )+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = MPa_perA)) +
  geom_jitter(aes(color=Type), alpha = .7,  )+
  theme_classic()+
  facet_wrap(~tree)

ggplot(data = fog, aes(x = Minutes, y = MPa_dif)) +
  geom_jitter(aes(color = height,shape = tree ), alpha = .7 )+
  theme_light()+
  scale_color_viridis()+
  facet_wrap(~Type)

#boxplot

ggplot(data = fog, aes(group=Type, y = weight_perA)) +
  geom_boxplot()+
  facet_wrap(~tree)

ggplot(data = fog, aes(group=tree, y = weight_perA)) +
  geom_boxplot()+
  facet_wrap(~Type)

