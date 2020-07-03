library(MASS)
library(dplyr)
help(Cars93)
table(Cars93$DriveTrain)
table(Cars93$DriveTrain, Cars93$Origin)
par(mfrow = c(2,2))
## 1A

cars = select(Cars93, Price, EngineSize, Horsepower, DriveTrain, Length, Weight, Origin, MPG.highway)
mpghw.lm=lm(MPG.highway ~. *Origin-DriveTrain:Origin + I(Weight^2), data = cars)
summary(mpghw.lm)
plot(mpghw.lm) ## 42, 73, 39 
Cars93[57,] # Mazda RX
Cars93[42,]
# take out outliers
cars = cars[-57,]
## reffiting the model
cars2 = select(Cars93, Price, EngineSize, Horsepower, DriveTrain, Length, Weight, Origin, MPG.highway)
n = dim(cars2)[1]
mpghw.lm=lm(MPG.highway ~. *Origin-DriveTrain:Origin + I(Weight^2), data = cars2)
mpghwBIC.lm= step(mpghw.lm, k = log(n))
summary(mpghwBIC.lm)
plot(mpghwBIC.lm) # 39 stands out 
Cars93[39,]
## reduce model using bic and summarize
HWBIC.lm = step(mpghw.lm, k = log(n))
summary(HWBIC.lm)
plot(HWBIC.lm) #39 & 42 both are high
Cars93[39,] # Geo Metro
Cars93[42,] # Honda Civic Small
Cars93[31,] # High leverage ford festiva

## 1B

cars3 = select(Cars93, Price, EngineSize, Horsepower, DriveTrain, Length, Weight, Origin, MPG.city)
mpgCity.lm=lm(MPG.city ~. *Origin-DriveTrain:Origin + I(Weight^2), data = cars3)
summary(mpgCity.lm)
plot(mpgCity.lm)
cars3 = cars[-57,]
x = dfbetas(mpgCity.lm)[39,] 
# refit model
cars4 = select(Cars93, Price, EngineSize, Horsepower, DriveTrain, Length, Weight, Origin, MPG.city)
n1 = dim(cars4)[1]
mpgCity.lm=lm(MPG.city ~. *Origin-DriveTrain:Origin + I(Weight^2), data = cars4)
mpgCityBIC.lm= step(mpgCity.lm, k = log(n1))
summary(mpgCityBIC.lm)
plot(mpgCityBIC.lm) ## 42 & 39 are high again
Cars93[31,]

## reduce model using bic and summarize
filter(cars4, Horsepower > 250)
CityBIC.lm = step(mpgCity.lm, k = log(n1))
summary(CityBIC.lm)
plot(CityBIC.lm)
Cars93[39,] # Geo Metro has high leverage and residual
Cars93[19,] # civic has high residual 

## 1C

cars5 = select(Cars93, MPG.highway, MPG.city, EngineSize, Horsepower, DriveTrain, Length, Weight, Origin, Price)
Price.lm=lm(Price ~. *Origin-DriveTrain:Origin + I(Weight^2), data = cars5)
summary(Price.lm)
plot(Price.lm)
Cars93[59, ] #Mercedes Benz 300E has high residual

## reduce model
PriceBIC.lm = step(Price.lm, k = log(n1))
summary(PriceBIC.lm)
plot(PriceBIC.lm) ## 59 has a high price, but not alot of benefits

                ## 2

## 2A

    # Fit a linear model to predict medv based on the independent variables:
    ## Seleting all varibles from data boston
boston = select(Boston, medv, crim, chas, nox, rm, tax, lstat)
boston$chas = factor(boston$chas)
boston$lstat = log(1-boston$lstat/100)/(boston$lstat/100)
## fitting linear model of medv vs all varibles 
medv.lm = lm(medv ~ .*chas, data = boston)
plot(medv.lm) ## 373 has high resisdual , since plot is bowl we apply quadratic term

summary(boston)
levels(boston$chas) 
Boston[357,]
## 2B


bnames = names(boston)[-1]
resi = residuals(medv.lm)
par(mfrow = c(2,3))
for (inames in bnames) {
plot(resi ~boston[,inames], xlab=inames, pch=19, col="blue", main="Residual of Full Fit vs iNames" )  
abline(h=0, col= "cyan")
} 
n1 = dim(Boston)[1]
xcol = rep("black", n)
bndx = c(357,365)
xcol[bndx]= "red"
xpch=rep(20,n)
xpch[bndx]= 19
pairs(x[, c("crim", "nox", "rm", "tax", "lstat")], pch = xpch, col = xcol)

#plots look good
summary(medv.lm)




  
    ## reduce using BIC 
n = nrow(boston)

boston.bic= step(medv.lm, k=log(n))
summary(boston.bic)
par(ask=FALSE)
    ## add a quadratic form
medv2.lm= lm(medv ~.*chas+ I(rm^2), data = boston)
summary(medv2.lm)
par(mfrow = c(2,2))
plot(medv2.lm)
dim(boston)
boston$chas = factor(boston$chas) # convert it to a factor
boston[1,]
boston[,1]

## 2C

      ## reduce model using BIC and interpret
medv.bic = step(medv2.lm, k=log(n)) 
plot(medv.bic) # 372, 369, and 365 have high residulas on residulas vs fitted line
q= 2/sqrt(n)
x = abs(dfbetas(medv.bic)) # gives u the change in coefficeint with and without the observation
                #abs value needed bc of magnitude
r=x>q
r[357, ]
r[365, ]
boston[357,] 
n=dim(Boston)[1]
xcol=rep("black", n)
bndx=c(357,365)
xcol[bndx]="red"
xpch=rep(20,n)
xpch[bndx]=19
pairs(boston[,c("crim", "nox", "rm", "tax", "lstat")], pch=xpch, col = xcol) # see where these observations fit in that multivariate space
summary(medv.bic) #2.54 as coeffeicent for crim:chas

## 2D

        ## 95% CI for predicted medv 
## selecting houses by the river and not by the river 
boston2=boston[1:2,]
boston2$crim = mean(boston$crim)
boston2$chas = c(0,1)
boston2$medv = NULL
boston2$nox = mean(boston$nox)
boston2$rm = mean(boston$rm)
boston2$tax = mean(boston$tax)
boston2$lstat = mean(boston2$lstat)
boston2$chas = factor(boston2$chas)
boston2 # new data that we will be using to predict
        # now construct a 95% confidence interval
predict(medv.bic, newdata = boston2, interval = "confidence")

            ## 3

# 3A
hospital = read.table("http://www.utdallas.edu/~ammann/stat4355scripts/SENIC.csv", header = TRUE, sep = ",")
dimnames(hospital)
risk.lm = lm(Infection ~. +MedSchool*Culturing+MedSchool*Xray+MedSchool*Nurses, data = hospital)
summary(risk.lm)
plot(risk.lm)
        # only 8 has high leverage and 53 has high residual, normality is resonable from qqplot 
        # normaility is resonable based on qq plot, its almost exact normaility
# 3B

#reducing model using BIC
n = dim(hospital)[1]
risk.lmBIC= step(risk.lm, k = log(n))
summary(risk.lmBIC)
plot(risk.lmBIC)
## all increase chance of getting an infection, slope is .188
## basically the longer you stay, higher chance of infection
## cenus beds and nurses have p strong correaltion to facilites, which is why they were removed.
## aic more interested in fitting, bic reduces bias
# 3C

hospital2 = select(hospital, Stay, Age, Culturing, Xray, Beds, MedSchool, Region, Census, Nurses, Facilities)
hospital2=hospital[1,]
hospital2$Stay= 12
hospital2$Age= 55
hospital2$Culturing= 12
hospital2$Xray= 75
hospital2$Beds= 550
hospital2$MedSchool= "Yes"
hospital2$Region= "S"
hospital2$Census= 475
hospital2$Nurses= 525
hospital2$Facilities= 65
hospital2$Infection=NULL
## 4.998 is mean
predict(risk.lmBIC, newdata = hospital2, interval = "prediction")
mean(hospital$Infection) ## 4.35 is mean of overall
# predicted > mean value


        ## 4
library(car)
inft =read.table("http://www.utdallas.edu/~ammann/stat4355scripts/infection.dat", header = TRUE, sep = "")
inft[sapply(inft, is.numeric)]
lapply(inft[sapply(inft, is.numeric)], as.factor)
# hospital one is the best 
#postive is bad 
  
  # 4A

            ## fit model w/o interaction
inft$Outcome= factor(inft$Outcome)
inft$Hospital= factor(inft$Hospital)
dimnames(inft)
o.glm=glm(Outcome~., data= inft, family=binomial) #binomial bc of logistic regression
summary(o.glm)
plot(o.glm)

            ##fit model w interaction
oc.glm = glm(Outcome~Severity*Hospital, data=inft, family=binomial) ## binomial bc of logistic regression
summary(oc.glm)
plot(oc.glm)

          ## reduce model w interaction
n=dim(inft)[1]
ocBIC.lm= step(oc.glm, k=log(n))
summary(ocBIC.lm)
          # approximate r squred for this model = (null - residual) / null = (67.745 - 34.676)/67.745 = 0.4881393
          # R-squared = 48%
plot(oc.glm)
residualPlots(oc.glm)
        # only keep the additive model meaning that interaction between severity and hospital is not significant

##4B

inft2= inft
inft2= inft[1:3,]
inft2$Severity = mean(inft$Severity)
inft2$Hospital= c("1", "2", "3")
inft2$Outcome= NULL
inft2
out.pred = predict.glm(ocBIC.lm, newdata=inft2, se.fit = TRUE, type = "link")
out.pred
# conf. int = (pred +- t * se.fit)
sum.infection = summary(ocBIC.lm)
sum.infection$df[2] # df for residual deviance
t = qt(.975,45)
# 2.014103
con1= c(-2.91, 0.06)
con2= c(round((-0.4420182-2.01403)*0.6842137), round((-0.4420182+2.01403)*0.6842137))
con3= c(round((1.9411900-2.01403)*0.6842137), round((1.9411900+2.01403)*0.6842137))
(hos1= round(binomial()$linkinv(con1),2))
(hos2= round(binomial()$linkinv(con2),2))
(hos3= round(binomial()$linkinv(con3),2))
# treat outcome closer to 1 is bad (didn't get the treatment)
# hospital1 is the best bc its chance of having a bad outcome is 51%

##4C
par(mfrow = c(1,1))
pcol = c("red","DarkGreen","blue")
plot(x=inft$Severity, y=inft$Outcome == 1, type="p", xlim = c(0,200))
out.pred = predict(ocBIC.lm, newdata=inft2, se.fit = TRUE, type = "link")
Severity.range = range(inft$Severity)
s1 = data.frame(Severity=seq(Severity.range[1],Severity.range[2],length=250), Hospital = as.factor(1))
s2 = data.frame(Severity=seq(Severity.range[1],Severity.range[2],length=250), Hospital = as.factor(2))
s3 = data.frame(Severity=seq(Severity.range[1],Severity.range[2],length=250), Hospital = as.factor(3))

# Hospital 1
out.pred2 = predict.glm(ocBIC.lm, newdata=s1, se.fit = TRUE, type = "link")
h1_fit = binomial()$linkinv(out.pred2$fit) # bionomial needed to convert it to probability
conf_h1_low = binomial()$linkinv(out.pred2$fit - out.pred2$se.fit*t) # lower bound
conf_h1_high = binomial()$linkinv(out.pred2$fit + out.pred2$se.fit*t) # upper bound
confh1 = data.frame(conf_h1_low, conf_h1_high) # combine those two above
confh1 # confidence interval for hosptial 1
plot(x=inft$Severity, y=inft$Outcome == 1, type="p", xlim=c(0,200))
title("Severity vs Outcome of Hospital 1\nwith 95% Prediction and Confidence Intervals")
lines(s1$Severity, h1_fit, col = pcol[1])
lines(s1$Severity, confh1$conf_h1_low, col=pcol[2])
lines(s1$Severity, confh1$conf_h1_high,col=pcol[2])
legend(x=150, y = 0.1, Severity.range[1],legend=c("Fit","Confidence"), lty=1,col=pcol,yjust=0,cex=.8)

# Hospital 2
pcol = c("Cyan","Maroon","Salmon")
out.pred2 = predict.glm(ocBIC.lm, newdata=s2, se.fit = TRUE, type = "link")
h2_fit = binomial()$linkinv(out.pred2$fit) # bionomial needed to convert it to probability
conf_h2_low = binomial()$linkinv(out.pred2$fit - out.pred2$se.fit*t) # lower bound
conf_h2_high = binomial()$linkinv(out.pred2$fit + out.pred2$se.fit*t) # upper bound
confh2 = data.frame(conf_h2_low, conf_h2_high) # combine those two above
confh2 # confidence interval for hosptial 1
plot(x=inft$Severity, y=inft$Outcome == 1, type="p", xlim=c(0,200))
title("Severity vs Outcome of Hospital 2\nwith 95% Prediction and Confidence Intervals")
lines(s2$Severity, h2_fit, col = pcol[1])
lines(s2$Severity, confh2$conf_h2_low, col=pcol[2])
lines(s2$Severity, confh2$conf_h2_high,col=pcol[2])
legend(x=150, y = 0.1, Severity.range[1],legend=c("Fit","Confidence"), lty=1,col=pcol,yjust=0,cex=.8)

# Hospital 3
pcol = c("Purple","Blue","Orange")
out.pred2 = predict.glm(severityBIC.lm, newdata=s3, se.fit = TRUE, type = "link")
h3_fit = binomial()$linkinv(out.pred2$fit) # bionomial needed to convert it to probability
conf_h3_low = binomial()$linkinv(out.pred2$fit - out.pred2$se.fit*t) # lower bound
conf_h3_high = binomial()$linkinv(out.pred2$fit + out.pred2$se.fit*t) # upper bound
confh3 = data.frame(conf_h3_low, conf_h3_high) # combine those two above
confh3 # confidence interval for hosptial 1
plot(x=infection$Severity, y=infection$Outcome == 1, type="p", xlim=c(0,200))
title("Severity vs Outcome of Hospital 3\nwith 95% Prediction and Confidence Intervals")
lines(s3$Severity, h3_fit, col = pcol[1])
lines(s3$Severity, confh3$conf_h3_low, col=pcol[2])
lines(s3$Severity, confh3$conf_h3_high,col=pcol[2])
legend(x=150, y = 0.1, Severity.range[1],legend=c("Fit","Confidence"), lty=1,col=pcol,yjust=0,cex=.8)

