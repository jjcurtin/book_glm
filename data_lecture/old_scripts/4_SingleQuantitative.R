#################Script for Unit 4- two parameter model############
library(lmSupport)


#################One parameter, mean-only, FPS model###########################
FilePath = 'G:/LectureDataR'
FileName = '4_SingleQuantitative_BAC_FPS.dat'
d = dfReadDat(file.path(FilePath,FileName))
str(d)      #get strucutre of data frame
head(d)
varDescribe(d)    


m1 = lm(FPS ~ 1, data=d)   #estimate one parameter model (m1- the compact model)
m2 = lm(FPS ~ BAC, data=d)  #estimate two parameter model (m2- the augmented model)

#Figures for 1 and 2 parameter models
windows()
par('cex' = 1.5,  'lwd'=2)
plot(jitter(rep(1,length(d$FPS)),2),d$FPS, axes=FALSE, xlim=c(0,2),xlab='')      #scatterplot
axis(2)  #add default Y axis
abline(h=coef(m1), col='blue', lwd=2)  #line at mean of FPS

plot(d$BAC,d$FPS)      #scatterplot
abline(h=coef(m1), col='blue', lwd=2)  #line at mean of FPS
abline(coef = coef(m2), col='red', lwd=2)  #add regression line from model


m2 = lm(FPS ~ BAC + 1, data = d)
modelSummary(m2)

pt(-1.92, 94, lower.tail = TRUE)*2  #get two tailed p-value for t-test for b1
pt(-1.92, 94, lower.tail = TRUE)  #get one tailed p-value for t-test for b1

#sampling distribution figure for b1
windows()
par('cex' = 1.5,  'lwd'=2)
t=summary(m2)     #save summary in t
b1 = seq(-400,400,.01)   #make a sequence
plot(x=b1,y=dt(b1/t$coefficients[2,2],m2$df.residual), type='l', col= 'red', lwd=2, main = 'Sampling Distribution for b1', xlab= 'b1', ylab= 'Probability')
abline(v=t$coefficients[2,1], col='black', lwd =2)
abline(v=-t$coefficients[2,1], col='black', lwd =2)

confint(m2)  #conidence intervals for b0 and b1

#model comparisons for B1
SSEC = sum(residuals(m1)^2)
SSEA = sum(residuals(m2)^2)

FStat = ((SSEC - SSEA)/(2-1)) / (SSEA/(96-2))
pf(FStat, 1,94, lower.tail=FALSE)


#model comparisons for b0
m1a = lm(FPS ~ BAC - 1, data=d)  #use -1 to REMOVE the intercept (i.e., set it equal to 0)
modelCompare(m1a,m2)
plot(d$BAC,d$FPS)      #scatterplot
abline(a=0, b=coef(m1a), col='blue', lwd=2)  #line at mean of FPS

#Centering BAC
d$BACC = d$BAC - mean(d$BAC)
varDescribe(d)
m2c = lm(FPS ~ BACC, data=d)
modelSummary((m2c))
plot(d$BACC,d$FPS)      #scatterplot
abline(coef = coef(m2c), col='red', lwd=2)  #add regression line from model


var(fitted.values(m2))/var(d$FPS)  #caluclate R2
modelR2(m2)

corr.test(d$BAC, d$FPS)  #calculate and test correlation coefficient




#Visualizing the model with Effects package
e = effect('BAC', m2)
plot(e)


#Publication quality figure: Reg line with error band for Y hat
windows()
par('cex' = 1.5,  'lwd'=2)
plot(d$BAC,d$FPS, xlim = c(0,.15), xlab = 'Blood Alcohol Concentration', ylab = 'Fear-potentiated startle')
dNew = data.frame(BAC= seq(0,.14,.0001))
pY = modelPredictions(m2,dNew)
lines(dNew$BAC,pY[,1], col='red', lwd=2)
lines(dNew$BAC,pY[,2], col='gray', lwd=1)
lines(dNew$BAC,pY[,3], col='gray', lwd=1)