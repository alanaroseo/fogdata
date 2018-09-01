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
  theme_classic()+
  facet_wrap(~tree)

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

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot()