#fog by height graphical analysis

library(tidyverse)
library(ggplot2)
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
  geom_point(aes(color=tree, shape = Type, alpha = height)) 

#boxplot

ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot()