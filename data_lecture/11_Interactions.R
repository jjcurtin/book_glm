rm(list=ls())   #clear all objects from workspace

library(lmSupport)
FilePath = 'G:/LectureDataR'
FileName = '11_Interactions_Jurors.dat'

d = dfReadDat(file.path(FilePath,FileName))

#simple descriptives
varDescribe(d)

#Additive model
mAdd = lm(Guilt ~ BlackDefend + Bias, data= d)
modelSummary(mAdd)
modelEffectSizes(mAdd)
confint(mAdd)

d$BlackDefendC = d$BlackDefend - .5
d$BiasC = d$Bias - mean(d$Bias)
head(d)
varDescribe(d)

mInt = lm(Guilt ~ BlackDefendC * BiasC, data=d)
modelSummary(mInt)
modelEffectSizes(mInt)
confint(mInt)


#Display raw additive model
windows()
dNew = data.frame(BlackDefend = 0,Bias = 5:60)
pYWhite = modelPredictions(mAdd,dNew)
dNew$BlackDefend = 1  #black
pYBlack = modelPredictions(mAdd,dNew)
plot(x=c(5,60), y=c(0,70), type='n', xlab = 'Juror Implicit Bias', ylab = 'Defendant Guilt', frame.plot=FALSE, cex.lab=1.5, cex.axis=1.2)
lines(x=dNew$Bias,y=pYWhite[,1], type='l', lty=1, col='blue', lwd=3)
lines(x=dNew$Bias,y=pYWhite[,2], type='l', lty=1, col='gray', lwd=1)
lines(x=dNew$Bias,y=pYWhite[,3], type='l', lty=1, col='gray', lwd=1)

lines(x=dNew$Bias,y=pYBlack[,1], type='l', lty=1, col='Red', lwd=3)
lines(x=dNew$Bias,y=pYBlack[,2], type='l', lty=1, col='gray', lwd=1)
lines(x=dNew$Bias,y=pYBlack[,3], type='l', lty=1, col='gray', lwd=1)
legend('topleft',c('White', 'Black'), col=c('blue','red'), lty=c(1,1), lwd=3, inset=.01,cex=1.25)


#Display raw interactive model
mIntR = lm(Guilt ~ BlackDefend*Bias, data=d)
dNew = data.frame(BlackDefend = 0,Bias = 5:60)
pYWhite = modelPredictions(mIntR,dNew)
dNew$BlackDefend = 1
pYBlack = modelPredictions(mIntR,dNew)
plot(x=c(5,60), y=c(0,70), type='n', xlab = 'Juror Implicit Bias', ylab = 'Defendant Guilt', frame.plot=FALSE, cex.lab=1.5, cex.axis=1.2)
lines(x=dNew$Bias,y=pYWhite[,1], type='l', lty=1, col='blue', lwd=3)
lines(x=dNew$Bias,y=pYWhite[,2], type='l', lty=1, col='gray', lwd=1)
lines(x=dNew$Bias,y=pYWhite[,3], type='l', lty=1, col='gray', lwd=1)

lines(x=dNew$Bias,y=pYBlack[,1], type='l', lty=1, col='Red', lwd=3)
lines(x=dNew$Bias,y=pYBlack[,2], type='l', lty=1, col='gray', lwd=1)
lines(x=dNew$Bias,y=pYBlack[,3], type='l', lty=1, col='gray', lwd=1)
legend('topleft',c('White', 'Black'), col=c('blue','red'), lty=c(1,1), lwd=3, inset=.01,cex=1.25)

#Main effect line for Bias
dNew$BlackDefend = 0.5
pYAverage = modelPredictions(mIntR,dNew)
pYAverage = modelPredictions(mIntR,dNew)
lines(x=dNew$Bias,y=pYAverage[,1], type='l', lty=1, col='black', lwd=3)

#main effect line for BlackDefend
abline(v=34.68, col='black', lwd=3)

#-1SD effect line for BlackDefend
abline(v=18.53, col='black', lwd=3)

#+1SD effect line for BlackDefend
abline(v=50.83, col='black', lwd=3)



#Simple effects of bias
mWhite = lm(Guilt ~ BlackDefend*BiasC, data=d)
modelSummary(mWhite)

d$WhiteDefend = 1-d$BlackDefend
mBlack = lm(Guilt ~ WhiteDefend*BiasC, data=d)
modelSummary(mBlack)

#Simple effects of Defendant race
d$BiasH = d$BiasC -   sd(d$BiasC)
mBiasH = lm(Guilt ~ BlackDefendC*BiasH, data=d)
modelSummary(mBiasH)
modelEffectSizes(mBiasH)
confint(mBiasH)

d$BiasL = d$BiasC +   sd(d$BiasC)
mBiasL = lm(Guilt ~ BlackDefendC*BiasL, data=d)
modelSummary(mBiasL)
modelEffectSizes(mBiasL)
confint(mBiasL)