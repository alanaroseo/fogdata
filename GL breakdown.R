GL<- as.data.frame(read.delim("clipboard"))


library(tidyverse)
library(ggplot2)
library("viridis")

names(GL)

ggplot(data = GL, aes(x = rel_ht, y = percent_G )) +
  geom_point(aes(color = tree), alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, color = "orchid")+
  facet_wrap(~forest)

ggplot(data = GL, aes(x = rel_ht, y = percent_G )) +
  geom_boxplot(aes(color= branch_ht))+
  theme_classic()+
  facet_wrap(~forest)

