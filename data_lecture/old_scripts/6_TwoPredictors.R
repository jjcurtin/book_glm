# Opening and Exploring the Data -----------------------------------
FilePath = 'G:/LectureDataR' 
FileName = '6_TwoPredictors_FPS.dat'
d = dfReadDat(file.path(FilePath,FileName))                      

head(d)   #look at a few cases
str(d)   #look at structure of data frame
varDescribe(d, 2)    #moderate detail descriptive stats

windows()  #new window on PC
par('cex' = 1.5,  'lwd'=2)
varPlot(d$FPS, 'FPS')   #histograms of all variables
varPlot(d$BAC, 'BAC')
varPlot(d$TA, 'TA')

corr.test(d)  #see also cor for simple correlations without p-values
print(cor(d),digits=2)  #display correlation table with fewer (about 2) digits

plot(d$BAC,d$FPS)
plot(d$TA,d$FPS)
plot(d$BAC, d$TA)

library(car)
spm(d)  #scatterplot matrix




#Models --------------------------------
m2 = lm(FPS ~ BAC, data=d)  #two parameter model with BAC
modelSummary(m2)
modelEffectSizes(m2)
modelR2(m2)
confint(m2)

m3 = lm(FPS ~ BAC + TA, data=d)   #three parameter model with both BAC and TA
modelSummary(m3)
modelEffectSizes(m3)
modelR2(m3)
confint(m3)
vif(m3)

#two parameter model with only TA (compact model for testing BAC)
m2c = lm(FPS ~ TA, data=d)  
modelSummary(m2c)
modelEffectSizes(m2c)
modelR2(m2c)

#Fit 3 parameter model with mean-centered TA
d$TAC = d$TA - mean(d$TA)
m3Centered = lm(FPS ~ BAC + TAC, data = d)
modelSummary(m3Centered)





#Sampling distribution plots for powerpoint slides-----------------------
windows()
par('cex' = 1.5,  'lwd'=2)

SE = 86.6
B=seq(-4 * SE,4 * SE,.01)  #plot sampling distribution
t=B/SE
plot(x=B,y=dt(t,94), type='l', col= 'red', xlab= 'B', ylab= 'Probability')
abline(v=-177.1, col='black')

#Visualizing models-------------------------------
#Display 3d 2 predictor model (not recommended)
options (device=windows)    #or options(device==quartz) so that new windows open automatically
par('cex' = 1.5,  'lwd'=2)
library(scatterplot3d)
s3d <-scatterplot3d(c(0,.16),c(0,500),c(-20,100),type='n', xlab='Blood Alcohol Concentration (%)', ylab='Trait Anxiety', zlab= 'FPS', cex.lab = 1.5, cex.axis=1.2)
m3 = lm(FPS ~ BAC + TA, data=d)  #order of variables much match axes)
s3d$plane3d(m3, lty.box = "solid")     

#using the effects package (USE THIS)
library(effects)
e = effect('BAC', m3)  #Plot the BAC effect
plot(e)

e = effect('BAC*TA', m3, default.levels =3)  #Plot the BAC effect
plot(e, multiline=TRUE)

#Publication quality figure using base package and lmSupport
dP = data.frame(BAC = seq(0,.14, .001), TA = mean(d$TA))
dP = modelPredictions(m3,dP)

figNewDevice()
figPlotRegion(x=c(0,.14),y=c(-50,150))
figConfidenceBand(dP$BAC,dP$Predicted,dP$CILo,dP$CIHi)
figPoints(d$BAC,d$FPS)
figLines(dP$BAC,dP$Predicted)
figAxis(side=1,lab.text='Blood Alcohol Concentration (%)', scale.at=seq(0,.13, by=.01))
figAxis(side=2,lab.text=expression(bold(paste('Startle Potentiation (', mu, 'V)', sep=''))))



###########
#Correlated predictors
FileName = '6_TwoPredictors_Exam.dat'
FilePath = 'G:/LectureDataR'
d= dfReadDat(file.path(FilePath,FileName))

d$StudyHoursC = d$StudyHours - mean(d$StudyHours)
mExam1 = lm(Exam ~ StudyHoursC, data=d)
modelSummary(mExam1)

print(cor(d[,c('StudyHours', 'IQ', 'Exam')]), digits=2)

mExam2 = lm(Exam ~ StudyHoursC + IQ, data=d)
modelSummary(mExam2)

mIQ = lm(IQ~ StudyHoursC, data=d)
modelSummary(mIQ)


###########
#Dichotomous predictor
FileName = '6_TwoPredictors_Intervention.dat'
FilePath = 'G:/LectureDataR'
d= dfReadDat(file.path(FilePath,FileName))


mDepress1 = lm(d$DepressPost ~ InterventionGroup, data = d)
modelSummary(mDepress1)

d$DepressBaseC = d$DepressBase - mean(d$DepressBase)
mDepress2 = lm(d$DepressPost ~ InterventionGroup + DepressBaseC, data = d)
modelSummary(mDepress2)


library(effects)
e = effect('InterventionGroup', mDepress2)  #Plot the Intervention effect
windows()
plot(e, multiline=TRUE)
modelR2(mDepress2)
modelEffectSizes(mDepress2)

e = effect('DepressBaseC', mDepress2, given.values=c(InterventionGroup=0, InterventionGroup=0))  #Plot the Intervention effect
windows()
plot(e, multiline=TRUE)

#Option 1:  Raw data
dP = data.frame(InterventionGroup=c(0,1), DepressBaseC = 0)
dP = modelPredictions(mDepress2,dP) 
Means = matrix(dP$Predicted, nrow=2,ncol=1, byrow=TRUE)
colnames(Means) = c('CES-D')
rownames(Means) = c('Standard of Care', 'New Intervention')
se = matrix(dP$SE, nrow=2,ncol=1, byrow=TRUE)

bars.col = c('white', 'gray')

figNewDevice()
figBarPlot(Means,ylim=c(0,60), lab.text='Intervention Group', se=se, bars.col=bars.col)
figAxis(side=2,lab.text='CES-D', scale.at=seq(0,60,by=20))
figPoints(jitter(rep(1.5,100), amount=.02),d$DepressPost[d$InterventionGroup==0])
figPoints(jitter(rep(3.5,100), amount=.02),d$DepressPost[d$InterventionGroup==1])

#Option 2: residuals
figNewDevice()
figBarPlot(Means,ylim=c(0,60), lab.text='Intervention Group', se=se, bars.col=bars.col)
figAxis(side=2,lab.text='CES-D', scale.at=seq(0,60,by=20))
figPoints(jitter(rep(1.5,100), amount=.02),dP$Predicted[1]+modelErrors(mDepress2)[mDepress2$model$InterventionGroup==0])
figPoints(jitter(rep(3.5,100), amount=.02),dP$Predicted[2]+modelErrors(mDepress2)[mDepress2$model$InterventionGroup==1])
