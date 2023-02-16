############################################################
# mortality
waterdata=read.table("water.txt",header=TRUE)
x=waterdata$Concentration
y=waterdata$Rate

plot(x,y,ylim=c(0,3000),xlab="Calcium concentration (mg/L)",
     ylab="Mortality rate (deaths per 100 000)",
     main="Pairs of measurements for 61 large towns")

# Fitting a simple linear regression model
my.lm=lm(y~x)

# Adding fitted regression line
abline(my.lm,col=2)

# Residuals
my.lm$residuals
residuals(my.lm)

# Residual plots
# Checking constant variance
plot(x,residuals(my.lm),xlab="Concentration",ylab="Residual",
     main="Residuals for waterdata")
abline(h=0,lty=2)

# Checking normality assumption
# Histogram
hist(residuals(my.lm))

# QQ-plot
qqnorm(residuals(my.lm))
qqline(residuals(my.lm))

# Standard plots from lm
plot(my.lm)

#################################################################################
# Sums of Squares
anova(my.lm)

my.anova=anova(my.lm)
attributes(my.anova)
my.anova$`Sum Sq`

# Different ways to obtain SStot
# Using definition
n=length(x)
(n-1)*var(y)

# Summing rows in ANOVA table
sum(my.anova$`Sum Sq`)
sum(my.anova[,2])

# Coefficient of determination
# by hand
my.anova
SStot=sum(my.anova[,2])
SSres=my.anova[2,2]
1-SSres/SStot

# with summary
summary(my.lm)

my.summary=summary(my.lm)
attributes(my.summary)
my.summary$r.squared

# By relation with correlation coefficient
cor(x,y)^2

#################################################################################
# Standard tests H0:alpha=0 and H0:beta=0
summary(my.lm)

#confidence intervals
?confint
confint(my.lm)
confint(my.lm,level=0.99)

