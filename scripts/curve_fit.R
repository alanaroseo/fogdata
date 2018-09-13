model <- lm(y_var ~ poly(x_var,3)) #poly = polynominal 3=number of parameters (order)

summary(model)#

model <- lm( data= fog, rel_w_dif ~ poly(Minutes,3))

model_lin <- lm( data= fog, rel_w_dif ~ Minutes)
summary(model_lin)

plot(model)

stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 3, raw=TRUE),colour="red")#add 3rd order polynominal trendline to ggplot

#display polynominal equation :
library(polynom)

my.formula <- rel_w_dif ~ poly(Minutes, 3, raw = TRUE)#name a form for equation

m <- lm(my.formula, fog)
my.eq <- as.character(signif(as.polynomial(coef(m)), 3))


my.eq#display equation in console


#now to add to plot:
+annotate(geom = "text", x = 0.2, y = 15000, label = label.text, #example version giving x and y coordinates for lable placement
          family = "serif", hjust = 0, parse = TRUE, size = 4)

+annotate(geom = "text", label = label.text, 
          family = "serif", hjust = 0, parse = TRUE, size = 4)