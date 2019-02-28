GL<- as.data.frame(read.delim("clipboard"))


library(tidyverse)
library(ggplot2)
library("viridis")

names(GL)

ggplot(data = GL, aes(x = rel_ht, y = percent_G )) +
  geom_point( alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, aes(color = site))+
  facet_wrap(~forest)

ggplot(data = GL, aes(x = branch_ht, y = percent_G )) +
  geom_point( aes(color = site),alpha = .7)+
  theme_classic()+
  geom_smooth(method=lm, se=FALSE, aes(color = site))+
  facet_wrap(~forest)

ggplot(data = GL, aes(x = rel_ht, y = percent_G )) +
  geom_boxplot(aes(color= site,group=site), alpha = .3)+
  theme_classic()+
  facet_wrap(~forest)

plot(percent_G~forest, data =GL)

t.test(percent_G~forest, data =GL)

plot(percent_G~site, data =GL)
