################################################################
#Birth Control example
################################################################
library(lmSupport)

#Open BirthControl.sav data
FilePath = 'G:/LectureDataR'
FileName = '10_Interactions_BirthControl.dat'
d = dfReadDat(file.path(FilePath,FileName))

str(d)
#simple descriptives
varDescribe(d)
cor(d)

#Bivariate and additive models
mAtt = lm(BC ~ Att, data=d)
modelSummary(mAtt)

mPP = lm(BC ~ PP, data=d)
modelSummary(mPP) 

mAdd = lm(BC ~ Att + PP, data=d)
modelSummary(mAdd) 


#Display additive Att effect at three levels (1,3,5) of other variable (PP)
windows()
dNew = data.frame(Att = 1:5, PP = 1)
pY1 = modelPredictions(mAdd,dNew)
dNew$PP = 3
pY3 = modelPredictions(mAdd,dNew)
dNew$PP = 5
pY5 = modelPredictions(mAdd,dNew)
plot(x=c(1,5),y=c(0,25), type='n', xlab = 'Attitude about Birth Control', ylab = 'Birth Control Use', frame.plot=FALSE, cex.lab=1.5, cex.axis=1.2)
lines(x=dNew$Att,y=pY1$Predicted, type='l', lty=1, col='blue', lwd=2)
lines(x=dNew$Att,y=pY3$Predicted, type='l', lty=2, col='red', lwd=2)
lines(x=dNew$Att,y=pY5$Predicted, type='l', lty=3, col='green', lwd=2)
legend('topleft',c('PP = 1', 'PP = 3', 'PP = 5'), col=c('blue','red', 'green'), lty=c(1,2,3), lwd=2, inset=.01,cex=1.25)


#Additive model with centered IVs
d$AttC = d$Att - mean(d$Att)
d$PPC= d$PP - mean(d$PP)
mAddC= lm(BC ~ AttC + PPC, data=d)
modelSummary(mAddC)

#interactive model without cetnered IVs
mInt = lm(BC ~ Att*PP, data=d)
modelSummary(mInt)
confint(mInt)
modelEffectSizes(mInt)

#Centered Interactive model
mIntC = lm(BC ~ AttC*PPC, data=d)
modelSummary(mIntC)
confint(mIntC)
modelEffectSizes(mIntC)

#Display interactive Att effect at three levels (1,3,5) of other variable (PP)
dNew = data.frame(Att = 1:5, PP = 1)
mInt = lm(BC ~ Att*PP, data=dBC)
pY1 = modelPredictions(mInt,dNew)
dNew$PP = 3
pY3 = modelPredictions(mInt,dNew)
dNew$PP = 5
pY5 = modelPredictions(mInt,dNew)
plot(x=c(1,5),y=c(0,25), type='n', xlab = 'Attitude about Birth Control', ylab = 'Birth Control Use', frame.plot=FALSE, cex.lab=1.5, cex.axis=1.2)
lines(x=dNew$Att,y=pY1$Predicted, type='l', lty=1, col='blue', lwd=2)
lines(x=dNew$Att,y=pY3$Predicted, type='l', lty=2, col='red', lwd=2)
lines(x=dNew$Att,y=pY5$Predicted, type='l', lty=3, col='green', lwd=2)
legend('topleft',c('PP = 1', 'PP = 3', 'PP = 5'), col=c('blue','red', 'green'), lty=c(1,2,3), lwd=2, inset=.01,cex=1.25)

#Raw Interactive model
mInt = lm(BC ~ Att*PP, data=d)
modelSummary(mInt)

#Display interactive Att effect at 6 levels (including 0) of other variable (PP)
dNew = data.frame(Att = 0:5, PP = 0)
pY0 = modelPredictions(mInt,dNew)
dNew$PP = 1
pY1 = modelPredictions(mInt,dNew)
dNew$PP = 2
pY2 = modelPredictions(mInt,dNew)
dNew$PP = 3
pY3 = modelPredictions(mInt,dNew)
dNew$PP = 4
pY4 = modelPredictions(mInt,dNew)
dNew$PP = 5
pY5 = modelPredictions(mInt,dNew)
plot(x=c(0,5),y=c(0,25), type='n', xlab = 'Attitude about Birth Control', ylab = 'Birth Control Use', frame.plot=FALSE, cex.lab=1.5, cex.axis=1.2)
lines(x=dNew$Att,y=pY0$Predicted, type='l', lty=1, col='gray', lwd=3)
lines(x=dNew$Att,y=pY1$Predicted, type='l', lty=1, col='blue', lwd=3)
lines(x=dNew$Att,y=pY2$Predicted, type='l', lty=1, col='yellow', lwd=3)
lines(x=dNew$Att,y=pY3$Predicted, type='l', lty=1, col='red', lwd=3)
lines(x=dNew$Att,y=pY4$Predicted, type='l', lty=1, col='black', lwd=3)
lines(x=dNew$Att,y=pY5$Predicted, type='l', lty=1, col='green', lwd=3)
legend('topleft',c('PP = 0***', 'PP = 1', 'PP = 2','PP = 3','PP = 4','PP = 5'), col=c('gray', 'blue', 'yellow', 'red', 'black', 'green'), lty=1, lwd=3, inset=.01,cex=1.25)


#Simple effects of Att at PP levels
d$PP1 = d$PP -1
mInt_PP1 = lm(BC ~ Att*PP1, data=d)
modelSummary(mInt_PP1)
confint(mInt_PP1)
modelEffectSizes(mInt_PP1)

d$PP5 = d$PP -5
mInt_PP5 = lm(BC ~ Att*PP5, data=d)
modelSummary(mInt_PP5)
confint(mInt_PP5)
modelEffectSizes(mInt_PP5)


####PUB QUALITY FIG
mInt = lm(BC ~ Att*PP, data=d)
modelSummary(mInt)


dNew = data.frame(Att = 1:5, PP = 2)  #set up Xs for predictions for PP=2
pY2 = modelPredictions(mInt,dNew)
dNew$PP = 4 #update for PP=4
pY4 = modelPredictions(mInt,dNew)    

figNewDevice()   
figPlotRegion(x=c(1,5),y=c(0,25))
figConfidenceBand(pY2$Att,pY2$Predicted,pY2$CILo,pY2$CIHi)
figLines(pY2$Att,pY2$Predicted, lines.col='green')

figConfidenceBand(pY4$Att,pY4$Predicted,pY4$CILo,pY4$CIHi)
figLines(pY2$Att,pY4$Predicted, lines.col='blue')

figAxis(side=1,lab.text='Attitude about Birth Control', scale.at=seq(from=1,to=5,by=1))
figAxis(side=2,lab.text='Birth Control Use', scale.at=seq(from=0,to=25,by=5))
figLegend('topright', legend=c('Peer Pressure = 2','Peer Pressure = 4'),fill=c('green', 'blue'))
