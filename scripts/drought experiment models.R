#write.csv(data.frame(summary(mod)$coefficients), file="model_table.csv")

library("stargazer")
library(gridExtra)
library(grid)
library(investr)
library(AICcmodavg)
library(nlme)
library(lme4)
library(lmerTest)
library(lattice)

#coefficients(mod) # model coefficients
#confint(mod, level=0.95) # CIs for model parameters
#modted(mod) # predicted values
#residuals(mod) # residuals
#anova(mod) # anova table
#vcov(mod) # covariance matrix for model parameters
#influence(mod) # regression diagnostics 

# K-fold cross-validation:
library(DAAG)
#cv.lm(df=mydata, mod, m=3) # 3 fold cross-validation

#####
#TTv_per_m2

mod1 <- lm(TTv_per_m2 ~ ind_pred_gs, TT)
summary(mod1)
AICc(mod1)

mod6 <- lm(TTv_per_m2 ~ height, TT)
summary(mod6)
AICc(mod6)

anova(mod1,mod6)

mod7 <- lm(TTv_per_m2 ~ height, TTp)
summary(mod7)
AICc(mod7)


mod2 <- lm(TTv_per_m2 ~ ind_pred_gs+position, TT)
summary(mod2)
AICc(mod2)

lmm1 <- lmer(TTv_per_m2 ~ ind_pred_gs+(1|position), TT)
summary(lmm1)
AICc(lmm1)

lmm2 <- lmer(TTv_per_m2 ~ ind_pred_gs+(1|position), TTp)
summary(lmm2)
AICc(lmm2)

########################################################################
lmm9 <- lmer(TTv_per_m2 ~ ind_pred_gs+height+(1|position), TTp)#best, lowest AICc and most likely
summary(lmm9)
AICc(lmm9)
summary(lmm9)$sigma^2
summary(lmm9)$varcor$position[1]
#####################################################################

cv.lm(data=TTp, mod3, m=4)
anova(lmm9,lmm2)

lmm10 <- lmer(TTv_per_m2 ~ height+(1|position), TTp)
summary(lmm10)
AICc(lmm10)

lmm6 <- lmer(TTv_per_m2 ~ ind_pred_gs+(1|tree), TTp)
summary(lmm6)
AICc(lmm6)

anova(lmm2,lmm6)

lmm5 <- lmer(TTv_per_m2 ~ ind_pred_gs+height+(1|position), TTp)
summary(lmm5)
AICc(lmm5)

anova(lmm2,lmm5)

#######
#mol released per m2


mod3 <- lm(mol_rel.m2 ~ ind_pred_gs, TTp)
summary(mod3)
AICc(mod3)
summary(mod3)$sigma^2

lmm3 <- lmer(mol_rel.m2 ~ ind_pred_gs+(1|position), TT)
summary(lmm3)
AICc(lmm3)

lmm8 <- lmer(mol_rel.m2 ~ ind_pred_gs+(1|position), TTp)
summary(lmm8)
AICc(lmm8)
summary(lmm8)$sigma^2
summary(lmm8)$varcor$position[1]

anova(lmm8,lmm7)

lmm4 <- lmer(mol_rel.m2 ~ ind_pred_gs+(1|tree:position), TT)#best model structure with all data
summary(lmm4)
AICc(lmm4)
summary(lmm4)$sigma^2
summary(lmm4)$varcor$tree[1]

#############################################################################
lmm7 <- lmer(mol_rel.m2 ~ ind_pred_gs+(1|tree:position), TTp)#even better with paired dataset (varying intercepts only) top model for mol_rel.m2
summary(lmm7)
AICc(lmm7)
summary(lmm7)$sigma^2#residual variance should be low
summary(lmm7)$varcor$tree[1]#how var much is explained by tree
############################################################################

lmm10 <- lmer(mol_rel.m2 ~ ind_pred_gs+(1+tree|position), TTp)#more likely but higher AIC
summary(lmm10)
AICc(lmm10)
summary(lmm10)$sigma^2
summary(lmm10)$varcor$position[1]

anova(lmm7,lmm11)

mod4 <- lm(mol_rel.m2 ~ height, TT)
summary(mod4)
AICc(mod4)

mod5 <- lm(mol_rel.m2 ~ height, TTp)#neither related to height but paired data even less so
summary(mod5)
AICc(mod5)


lmm12 <- lmer(mol_rel.m2 ~ height+(1|tree:position), TTp)#decent fit but high AICc
summary(lmm12)
AICc(lmm12)

lmm14 <- lmer(mol_rel.m2 ~ ind_pred_gs+height+(1|tree:position), TTp)#the most var explained for mol rel per m2, looks like the best with AIC, but AICc is higher, BIC however is a tiny bit lower but by less than 2
summary(lmm14)
AICc(lmm14)
summary(lmm14)$sigma^2
summary(lmm14)$varcor$tree[1]


 anova(lmm14, lmm7)

######################
#seconds open
 mod8 <- lm(ind_sec_open ~ height, TTp)#best
 summary(mod8)
 AICc(mod8)
 summary(mod8)$sigma^2
 
 mod9 <- lm(ind_sec_open ~ TSF, TTp)
 summary(mod9)
 AICc(mod9)
 summary(mod9)$sigma^2

lmm15 <- lmer(ind_sec_open ~ height+(1|tree:position), TTp)
AICc(lmm15)
summary(lmm15)$sigma^2
summary(lmm15)$varcor$tree[1]

##################
#volume released per xylem volume
mod10 <- lm(vol_rel_per_xy_vol ~ height, TTp)
summary(mod10)
AICc(mod10)
summary(mod10)$sigma^2

lmm16 <- lmer(vol_rel_per_xy_vol ~ height+(1|tree:position), TTp)#nice
summary(lmm16)
AICc(lmm16)
summary(lmm16)$sigma^2
summary(lmm16)$varcor$tree[1]


lmm17 <- lmer(vol_rel_per_xy_vol ~ height+(0+height|tree), TTp)#better
summary(lmm17)
AICc(lmm17)
summary(lmm17)$sigma^2
summary(lmm17)$varcor$tree[1]

##################
mod11 <- lm(vol_rel_per_xy_vol ~ tree, TTp)#best model is tree alone, effect of height is small
summary(mod11)
AICc(mod11)
summary(mod11)$sigma^2
###############

mod12 <- lm(vol_rel_per_xy_vol ~ tree+(1|position), TTp)#very close 
summary(mod12)
AICc(mod12)
summary(mod12)$sigma^2


####################################################
#vol_rel

mod13 <- lm(vol_rel ~ xy_vol , TTp)#positive relationship to xy_vol, xy_vol is better related to height, but not by much
summary(mod13)
AICc(mod13)
summary(mod13)$sigma^2

mod14 <- lm(vol_rel ~ height , TTp)#better (negative) relationship to height
summary(mod14)
AICc(mod14)
summary(mod14)$sigma^2

###################################################################
mod15 <- lm(vol_rel ~ xy_vol+height , TTp)#best
summary(mod15)
AICc(mod15)
summary(mod15)$sigma^2
###################################################################

mod14 <- lm(xy_vol ~ height , TTp)#better (negative) relationship to height
summary(mod14)
AICc(mod14)
summary(mod14)$sigma^2

lmm18 <- lmer(vol_rel ~ xy_vol +height+(1|tree), TTp)#positive relationship to xy_vol, xy_vol is better related to height, but not by much
summary(lmm18)
AICc(lmm18)
summary(lmm18)$sigma^2