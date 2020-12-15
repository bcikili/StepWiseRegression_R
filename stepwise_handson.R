mydata <- read.csv('IQ.csv')
head(mydata)

fitdata <- lm(IQ ~., data = mydata)
summary(fitdata)

step(fitdata, direction = 'backward')
fwdata <- lm(IQ ~ Test1 + Test2 + Test4, data = mydata)
summary(fwdata)


data2 <- lm(IQ ~ 1, data = mydata)
summary(data2)

step(data2, direction = 'forward', scope =(formula(fitdata)) )

step(data2, direction = 'both', scope = (formula(fitdata)))
#Backward model gives lowest AIC score. 
# The multiple R squared value states that estimation variance of whole data is 38%, which pretty low. 
#P value of final backward model is not significant however individually only Test 4 is a significant predictor. 

#--------------stepwise regression 2 =----------------------------#

mystep <- read.csv('stepwiseRegression.csv')
glimpse(mystep)

fitstep1 <- lm(Y ~., data = mystep)
summary(fitstep1)
# overall p value is significant but only 4 of the IVs are significant the rest is not.

step(fitstep1, direction = 'backward')
bwstep <- lm(Y ~ X2 + X4 + X6 + X10 + X11 + X12, data = mystep)
summary(bwstep)
#AIC=213.38, Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9998


fitfw <- lm(Y ~ 1, data = mystep)
summary(fitfw)
step(fitfw, direction = 'forward', scope= formula(fitstep1))

finalfw <- lm (Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = mystep)
summary(finalfw)
#AIC=213.38, Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9998 

step(fitfw, direction = 'both', scope= formula(fitstep1))
#AIC=213.38
summary(lm(Y ~ X6 + X4 + X12 + X10 + X2 + X11, data = mystep))
# backward, forward and hybrid gave the same results for the given data.
# 99% of variance and 99% accuracy for all models with 