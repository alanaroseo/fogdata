#loading data
fog <- as.data.frame(read.delim("clipboard"))
L_all <- as.data.frame(read.delim("clipboard"))
G_all <- as.data.frame(read.delim("clipboard"))
names(G_all)

names(L_all)
#filtering the data into subsets

library(tidyverse)
library(dplyr)
# separating L and G
L_all <- filter(fog, Type == "L" )
G_all <- filter(fog, Type == "G" )
#Filtering by tree
T6_L <- filter(L_all, tree == "T6" )
T8_L <- filter(L_all, tree == "T8" )
T11_L <- filter(L_all, tree == "T11" )
T16_L <- filter(L_all, tree == "T16" )
T34_L <- filter(L_all, tree == "T34" )
T48_L <- filter(L_all, tree == "T48" )


T6_G <- filter(G_all, tree == "T6" )
T8_G <- filter(G_all, tree == "T8" )
T11_G <- filter(G_all, tree == "T11" )
T16_G <- filter(G_all, tree == "T16" )
T34_G <- filter(G_all, tree == "T34" )

#filter by sample

T6_L_20 <- filter(T6_L, height == "20" )
T6_L_45 <- filter(T6_L, height == "45" )
T6_L_91 <- filter(T6_L, height == "91" )

T6_G_20 <- filter(T6_G, height == "20" )
T6_G_45 <- filter(T6_G, height == "45" )
T6_G_91 <- filter(T6_G, height == "91" )
#
T8_L_50 <- filter(T8_L, height == "50.5" )
T8_L_66 <- filter(T8_L, height == "66.1" )
T8_L_80 <- filter(T8_L, height == "80.5" )
T8_L_97 <- filter(T8_L, height == "97" )

T8_G_50 <- filter(T8_G, height == "50.5" )
T8_G_66 <- filter(T8_G, height == "66.1" )
T8_G_80 <- filter(T8_G, height == "80.5" )
T8_G_97 <- filter(T8_G, height == "97" )


#
T11_L_30 <- filter(T11_L, height == "30" )
T11_L_50 <- filter(T11_L, height == "50.5" )
T11_L_90 <- filter(T11_L, height == "90" )

T11_G_30 <- filter(T11_G, height == "30" )
T11_G_50 <- filter(T11_G, height == "50.5" )
T11_G_90 <- filter(T11_G, height == "90" )
#
T16_L_37 <- filter(T16_L, height == "37.1" )
T16_L_51 <- filter(T16_L, height == "51" )

T16_G_37 <- filter(T16_G, height == "37.1" )
T16_G_51 <- filter(T16_G, height == "51" )

#
T34_L_22 <- filter(T34_L, height == "22.4" )
T34_L_56 <- filter(T34_L, height == "56.6" )
T34_L_80 <- filter(T34_L, height == "80.2" )
T34_L_102 <- filter(T34_L, height == "102.3" )

T34_G_22 <- filter(T34_G, height == "22.4" )
T34_G_56 <- filter(T34_G, height == "56.6" )
T34_G_80 <- filter(T34_G, height == "80.2" )
T34_G_102 <- filter(T34_G, height == "102.3" )

#
T48_L_60 <- filter(T48_L, height == "60.9" )

#####
names(T48_L_60)
