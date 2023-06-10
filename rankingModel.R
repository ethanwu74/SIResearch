require(MASS)
install.packages("ordinal")
library(ordinal)

#q12 q19 q23 q24 q31 q40 q46 q49 q69 q71 q77 q78 q80 q88
a = as.factor(x)
x= as.factor(x)
yrbss = cbind(yrbss, x)
yrbss = subset(yrbss, select = -c(x))
yrbss$q19 = as.factor(yrbss$q19)
yrbss$q19 = relevel(yrbss$q19, ref = 2)
yrbss$q23 = as.factor(yrbss$q23)
yrbss$q23 = relevel(yrbss$q23, ref = 2)
yrbss$q24 = as.factor(yrbss$q24)
yrbss$q24 = relevel(yrbss$q24, ref = 2)

model = polr(a ~ as.factor(yrbss$q12) + as.factor(yrbss$q19) + as.factor(yrbss$q23)
             + as.factor(yrbss$q24) + as.factor(yrbss$q31) + as.factor(yrbss$q40)
             + as.factor(yrbss$q46) + as.factor(yrbss$q49)  
             + as.factor(yrbss$q77) + as.factor(yrbss$q78)
             + as.factor(yrbss$q80) + as.factor(yrbss$q88))
summary(model)
summary(model1)

model1 = polr(droplevels(as.factor(yrbss$x)) ~., data = yrbss)
(ctable <- exp(coef(summary(model))))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
exp(confint.default(model))
exp(ctable)
ctable <- cbind(ctable, exp(confint.default(model)))

1 - pchisq(deviance(model), df.residual(model)) # IMPORTANT FOR FINDING SIGNIFICANCE
yrbss1 = yrbss
yrbss1 = na.omit(yrbss1)
plot_ordr(model)
