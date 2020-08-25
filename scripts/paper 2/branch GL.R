library(tidyverse)

GL <- as.data.frame(read.delim("clipboard"))
names(GL)



OG <- as.data.frame(read.delim("clipboard"))
names(OG)


# OG only -----------------------------------------------------------------


ns <- filter(OG, zone != "central" )


ggplot(data = ns, aes(x = Dominance, y = X..G.area)) +
  geom_boxplot(aes(color = Dominance), size=1,alpha = .7)+
  theme_classic()

ggplot(data = OG, aes(x = zone, y = X..G.area)) +
  geom_boxplot(aes(color = zone), size=1,alpha = .7)+
  geom_point(aes(color = zone), size=1,alpha = .7)+
  theme_classic()

ggplot(data = OG, aes(x = branch.height, y = X..G.area)) +
  geom_point(aes(color = zone), size=2,alpha = .7)+
  stat_smooth(aes(color = zone),method = lm, se=F)+
  theme_classic()

ggplot(data = ns, aes(x = rel.ht, y = X..G.area)) +
  geom_point(aes(color = zone), size=2,alpha = .7)+
  stat_smooth(aes(color = zone),method = lm, se=F)+
  theme_classic()

ggplot(data = OG, aes(x = rel.ht, y = X..G.area)) +
  geom_point(aes(color = zone), size=2,alpha = .7)+
  stat_smooth(aes(color = Dominance),method = lm, se=F)+
  theme_classic()

ggplot(data = OG, aes(x = branch.height, y = X..G.area)) +
  geom_point(aes(color = zone), size=2,alpha = .7)+
  stat_smooth(method = "auto", se=F)+
  theme_classic()


# plots -------------------------------------------------------------------
ggplot(data = GL, aes(x = Dominance, y = X..G.area)) +
  geom_boxplot(aes(color = Forest), size=1,alpha = .7)+
  theme_classic()

ggplot(data = OG, aes(x = zone, y = X..G.area)) +
  geom_boxplot(aes(color =zone), size=1,alpha = .7)+
  theme_classic()
###############
OG_GL <- filter(GL, Forest == "OG" )
SG_GL <- filter(GL, Forest == "2G" )


t.test(OG_GL$X..G.area, SG_GL$X..G.area, paired = FALSE, alternative = "two.sided")

var.test(OG_GL$X..G.area, SG_GL$X..G.area)#F test
########
dom_GL <- filter(GL, Dominance == "dominant" )
sup_GL <- filter(GL, Dominance == "suppressed" )


t.test(dom_GL$X..G.area, sup_GL$X..G.area, paired = FALSE, alternative = "two.sided")

var.test(dom_GL$X..G.area, sup_GL$X..G.area)#F test



############
OGd_GL <- filter(dom_GL, Forest == "OG" )
SGd_GL <- filter(dom_GL, Forest == "2G" )


t.test(OGd_GL$X..G.area, SGd_GL$X..G.area, paired = FALSE, alternative = "two.sided")

var.test(OG_GL$X..G.area, SG_GL$X..G.area)#F test