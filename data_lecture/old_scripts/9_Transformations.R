#demo of power transformation for skew
library(gvlma)
library(car)

options(device=windows)
windows(14,14)
par(mfrow = c(2,2))

Y = rchisq(n=500, df=1)    #Create positive skewed distribution
plot(density(Y), main = "Raw Y")
plot(density(bcPower(Y, .5)), main = "p = .5; sqrt(Y)")
plot(density(bcPower(Y, .25)), main = "p = .25")
plot(density(bcPower(Y, 0)), main = "p= 0; log(Y)")

#################################
#BAC example
ThePath = 'G:\LectureDataR'
d <-dfReadDat(file.path(ThePath,'9_Transformations_FPS.dat'))
str(d)
d = dfRemoveCases(d, c('0125'))

m = lm(FPS ~ BAC + TA + Sex, data=d)
modelSummary(m)
options(device='windows')
modelAssumptions(m,'Normal')
modelAssumptions(m,'Constant')
modelAssumptions(m,'Linear')
gvlma(m)

varDescribe(d)
d$FPS27 = d$FPS + 27
m2 = lm(FPS27 ~ BAC + TA + Sex, data=d)
modelBoxCox(m2)

d$FPS_BC = bcPower(d$FPS27,.5)
m3 = lm(FPS_BC ~ BAC + TA + Sex, data=d)
modelAssumptions(m3,'Normal')
modelAssumptions(m3,'Constant')
modelSummary(m3)
modelCorrectSE(m)
gvlma(m3)


#######################################
#Linearity example with fake data
dLinear = data.frame(X=1:100, Y = sqrt(1:100))  #make fake non-linear data
windows(21,7)
par(mfrow = c(1,3))
par(mar=c(5, 5, 5, 5) + 0.1)    # create extra margin room on the right for an axis 
plot(d$X,d$Y)  #original data
plot(sqrt(d$X), d$Y, xlab= "SQRT X", ylab = "Y")
plot(d$X, d$Y^2,, xlab= "X", ylab = "Y-Squared")

axis(side=4, at=sqrt(d$Y1), labels=(d$Y1))
mtext("Original Y1", side=4, line=3, cex=1.5)
plot(d$X^2, d$Y1, xlab= "X-squared", ylab = "Y")
axis(side=3, at=d$X^2, labels=(d$X)) 
mtext("Original X", side=3, line=3, cex=1.5)

plot(d$X,d$Y2)
plot(log2(d$X), d$Y2, xlab= "Log2(X)", ylab = "Y2")
axis(side=3, at=log2(d$X), labels=(d$X))
mtext("Original X", side=3, line=3, cex=1.5)
plot(d$X, d$Y2^2, xlab= "X", ylab = "Y2-squared")
axis(side=4, at=d$Y2^2, labels=(round(d$Y2,digits=1))) 
mtext("Original Y2", side=4, line=3, cex=1.5)
par(mar=c(5, 4, 4, 2) + 0.1)    #Return original margins


#######################################
#Linearity example with running data
d5K = dfReadDat(file.path(ThePath,'11_Transformations_5K.dat'))
str(d)
m5K = lm(Time ~ Age + Miles, data=d5K)
modelSummary(m5K)

modelAssumptions(m5K,'Normal')
modelAssumptions(m5K,'Constant')
modelAssumptions(m5K,'Linear')

d5K$logMiles = log2(d5K$Miles)
m5Ka = lm(Time ~ Age + logMiles, data=d5K)
modelSummary(m5Ka)
modelAssumptions(m5Ka,'Normal')
modelAssumptions(m5Ka,'Constant')
modelAssumptions(m5Ka,'Linear')

########################################
#graphing examples
#make fake data
X <- 3 * rchisq(200, df=3)
X_SR = sqrt(X)
Y = 3 * X_SR + rnorm(200,mean=0,sd = 1)

mRaw = lm(Y ~ X)
modelSummary(mRaw)
plot(X,Y)
abline(mRaw, col='red')

mSR = lm(Y ~ X_SR)
summary(mSR)
plot(X_SR,Y, xlab = 'Sqrt(X)')
abline(mSR, col='red')

#Plot with Raw X Scale on top axis
dP = data.frame(X_SR=seq(min(X_SR),max(X_SR),by=.01))
dP = modelPredictions(mSR,dP)
figNewDevice()   #default is for a window/mac window.  can also use tiff, or pdf as Type
figPlotRegion(x=c(0,6),y=c(0,20))
figPoints(X_SR,Y)
figLines(dP$X_SR,dP$Predicted)
figAxis(side=1,lab.text='SQRT(X)', scale.at=seq(from=0,to=6,by=1))
figAxis(side=2,lab.text='Y', scale.at=seq(from=0,to=20,by=5))
figAxis(side=3,lab.text='Raw X', scale.at=sqrt(seq(from=0,to=35,by=5)),scale.text=seq(from=0,to=35,by=5))


#Plot transformed model on raw data
dP = data.frame(X_SR=sqrt(seq(min(X),max(X),by=.1)))
dP = modelPredictions(mSR,dP)
figNewDevice()  
figPlotRegion(x=c(0,40),y=c(0,21))
figPoints(X,Y)
figLines(seq(min(X),max(X),by=.1),dP$Predicted)
figAxis(side=1,lab.text='X', scale.at=seq(from=0,to=40,by=5))
figAxis(side=2,lab.text='Y', scale.at=seq(from=0,to=20,by=5))