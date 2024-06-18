#################Script for Unit 3- One parameter, mean only models############
library(lmSupport)


#################One parameter, mean-only, FPS model###########################
FilePath = 'G:/LectureDataR'
FileName = '3_SingleMean_FPS.dat'
d = dfReadDat(file.path(FilePath,FileName))
str(d)      #get strucutre of data frame
head(d)

        
#Describe and display FPS
varDescribe(d)  #describe data
windows()  ##ONLY PC.  for mac use quartz() rather than windows()
par('cex' = 1.5,  'lwd'=2)       #setting for pretty graphs
hist(d$FPS)           #histogram


#Estimate One parameter model
m = lm(FPS ~ 1, data=d)           #fit linear model with only intercept
modelSummary(m)                        #report summary stats for linear model
modeErrors(m)  #report errors/residuals for each subject
coef(m)  #report model coefficients (parameter estimates)
modelPredictions(m)  #report predicted values


#Make figure for sampling distribution
windows()
par('cex' = 1.5,  'lwd'=2)
t=summary(m)     #save summary in t
b0 = seq(-40,40,.01)   #make a sequence
plot(x=b0,y=dt(b0/t$coefficients[2],m$df.residual), type='l', col= 'red', lwd=2, main = 'Sampling Distribution for b0', xlab= 'b0', ylab= 'Probability')
abline(v=t$coefficients[1], col='black', lwd =2)

#Get p-value for t
pt(8.40,95,lower.tail=FALSE) * 2       #t probabilities

#Calcuate SSEs for compact and augmented models
SSEC = sum((d$FPS - 0)^2)
SSEA = sum((d$FPS - coef(m)[1])^2)   #or more directly for this model: (sum(modeErrors(m)^2)

F = ((SSEC-SSEA)/(1-0))   / ((SSEA) / (96-1))  #calculate F for model comparison

pf(F,1,95, lower.tail=FALSE)  #get p-value for F


(PRE = (SSEC - SSEA)/ SSEC)   #Calculate paritial Eta2 or PRE

confint(m1,level=.95)         #confidence interval for b0

#plot for compact and agumented models
windows()
par('cex' = 1.5,  'lwd'=2)
plot(jitter(rep(1,length(d$FPS)),2),d$FPS, axes=FALSE, xlim=c(0,2),xlab='')      #scatterplot
axis(2)  #add default Y axis
abline(h=0, col='red', lwd=2)
abline(h=coef(m), col='blue', lwd=2)  #line at mean of FPS

t.test(d$FPS)   #one sample t-test