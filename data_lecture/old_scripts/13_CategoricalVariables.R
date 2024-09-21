library(lmSupport)
libary(car)   #for some()

FilePath = 'G:/LectureDataR'
d = dfReadDat(file.path(FilePath, '13_ThreeGroup.dat'))
View(d)

#simple descriptives by Group
tapply(d$Health, list(d$Group), 'mean')

#Dummy Codes (manual)
d$D1=varRecode(d$Group, c('AP','DP','HC'), c(1,0,0))
d$D2=varRecode(d$Group, c('AP','DP','HC'), c(0,1,0))
mDum= lm(Health ~ D1+ D2, data=d)
modelSummary(mDum)

#fit model with dummy codes using varContrasts, ref level = control (third) group
contrasts(d$Group) = varContrasts(d$Group, Type='Dummy', RefLevel = 3)
some(d)
mDum = lm(Health ~ Group, data=d)
modelSummary(mDum)
modelEffectSizes(mDum)  #Sum squares for model- only yields total group effect
Anova(mDum,type=3)

#fit model with dummy codes using varContrasts, ref level = control (third) group
contrasts(d$Group) = varContrasts(d$Group, Type='Dummy', RefLevel = 2)
some(d)
mDum2 = lm(Health ~ Group, data=d)
modelSummary(mDum2)
modelEffectSizes(mDum2)  #Sum squares for model- only yields total group effect

d = varRegressors(d,VarName = 'Group', RegressorNames = c('R1', 'R2'))
mR = lm(Health ~ R1 + R2, data=d)
modelSummary(mR)
modelEffectSizes(mR)

#John; coding preference fro understanding
d$C1A = varRecode(d$Group, c('AP', 'DP', 'HC'), c(.5, .5, -1))
d$C2A = varRecode(d$Group, c('AP', 'DP', 'HC'), c(1, -1, 0))
mA = lm(Health ~ C1A + C2A, data=d)
modelSummary(mA)

#John; coding preference for model
mB = lm(Health ~ C1B + C2B, data=d)
modelSummary(mB)


#Demo of Group as factor and use of contrasts
some(d)
d$Group = factor(d$Group, levels = c('AP','DP','HC'), labels = c('Alcoholic','Depressed', 'Control'))
class(d$Group)
levels(d$Group)
contrasts(d$Group)
contrasts(d$Group) = varContrasts(d$Group, Type = 'POC', POCList = list(c(.5,.5,-1), c(1,-1,0)), Labels =c('AD_v_C', 'A_v_D'))

mPOC = lm(Health ~ Group, data=d)
modelSummary(mPOC)

modelR2(mPOC)

#fit model with dummy codes, ref level = control (third) group
contrasts(d$Group) = varContrasts(d$Group, Type='Dummy', RefLevel = 3)
some(d)
mDum = lm(Health ~ Group, data=d)
modelSummary(mDum)
modelEffectSizes(m)  #Sum squares for model- only yields total group effect

#fit model with dummy codes using actual regressors coded into dataframe
d = lm.codeRegressors(d, VarName = 'Group', RegressorNames = c('D1', 'D2'))
m = lm(Health ~ D1 + D2, data = d)     #regression with mechanical regressors
summary(m)
modelEffectSizes(m)         #yields sum squares for regressor contrasts

#fit model with dummy codes, ref level = alcoholic (first) group
contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 1)
contrasts(d$Group)
m = lm(Health ~ Group, data=d)
summary(m)

d2 = subset(dH, Group != 'depressed')       #remove depressed participants
dim(d2)  #verify that selection worked
t.test(Health ~ Group, data=d2, var.equal=TRUE) #t-test on alcoholic vs. control

#Getting group mean predictions for the three groups by fitting different ref groups
contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 1)
mAlc = lm(Health ~ Group, data=d)
summary(mAlc)
contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 2)
mDep = lm(Health ~ Group, data=d)
summary(mDep)
contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 3)
mCon = lm(Health ~ Group, data=d)
summary(mCon)

#make simple bar graph with SE of mean as error bars (not best option for SE)
library(sciplot)
bargraph.CI(x.factor=d$Group, response=d$Health, xlab = "Patient Group", ylab = "Health", ylim=c(0,10))

#3-D plot.  not recommended but useful for conceptual understanding
library(scatterplot3d)                  
s3d <-scatterplot3d(d$X1,d$X2,dH$Health,type='p', xlab='X1', x.ticklabs = c('0', '','', '', '', '1'),y.ticklabs = c('0', '','', '', '', '1'), ylab='X2', zlab= 'Health', cex.lab = 1.5, cex.axis=1.5, pch=20, cex.symbols=1.5, color='blue')
s3d$plane3d(m, cex=1.25)
   

#Recommended figure
dNew = data.frame(Group = c('Alcoholic', 'Depressed', 'Control'))
pMeans = modelPredictions(mDum, dNew)
par(lwd=2, cex = 1.5, font=2,  font.axis=2, cex.lab = 1.2, font.lab=2) 
BarColors = c('gray', 'white', 'black')
BarDensity = c(-1,-1,10)  #negative density suppresses lines
BarAngle = c(0,0,45) #45 will produce diagonal hatch
barplot2(t(pMeans[,1]), beside = TRUE, names.arg = c('Alcoholic', 'Depressed', 'Control'), ylim = c(0,10), xlab = 'Patient Group', ylab = 'Health', plot.ci =TRUE, ci.l = t(pMeans[,2]), ci.u = t(pMeans[,3]),  ci.lwd = 3, col= BarColors, density=BarDensity, angle=BarAngle)

#Linear model using POCs
contrasts(d$Group) = varContrasts(d$Group, Type = 'POC', POCList = list(c(1,1,-2), c(1,-1,0)), Labels = c('Alc/Dep vs. Con', 'Alc vs. Dep'))  #recode with alcoholic as ref group
mPOC = lm(Health ~ Group, data=d)
summary(mPOC)


#Demo of nature of orthogonal contrasts
contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 1)
d = lm.codeRegressors(d, VarName = 'Group', RegressorNames = c('dumAlc1', 'dumAlc2'))
mAlc = lm(Health ~ dumAlc1 + dumAlc2, data=d)
lm.sumSquares(mAlc)

contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 2)
d = lm.codeRegressors(d, VarName = 'Group', RegressorNames = c('dumDep1', 'dumDep2'))
mDep = lm(Health ~ dumDep1 + dumDep2, data=d)
lm.sumSquares(mDep)

contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'DUMMY', RefLevel = 3)
d = lm.codeRegressors(d, VarName = 'Group', RegressorNames = c('dumCon1', 'dumCon2'))
mCon = lm(Health ~ dumCon1 + dumCon2, data=d)
lm.sumSquares(mCon)

contrasts(d$Group) = lm.setContrasts(d$Group, Type = 'POC', POCList = list(c(1,1,-2), c(1,-1,0)))
d = lm.codeRegressors(d, VarName = 'Group', RegressorNames = c('poc1', 'poc2'))
mPOC = lm(Health ~ poc1+ poc2, data=d)
lm.sumSquares(mPOC)



#family error corrections
p = c(.001, .009, .005, .100, .200, .040, .011)
p
p.adjust(p, method= 'bonferroni')
p.adjust(p, method= 'holm')



########################################
#Unequal N- Weighted coefficients
setwd('P:\\CourseWebsites\\PSY710\\Data\\CategoricalVariables')
d = lm.readDat('UnequalN.dat')

table(d$Group)  #note n in each group
mean(d$DV)
describe.by(d$DV, d$Group) 
#lm.describeGroups(d$DV,list(d$Group))  #An alternative to describe.by
contrasts(d$Group) = lm.setContrasts(d$Group, Type='Dummy', RefLevel =1) 
mDummy = lm(DV ~ Group, data=d)
summary(mDummy)

contrasts(d$Group) = lm.setContrasts(d$Group, Type='POC', POCList = list(c(-1,1))) 
mPOC = lm(DV ~ Group, data=d)
summary(mPOC)
plot(jitter(as.numeric(d$Group)-1.5, factor=.15), d$DV, xlim=c(-1,1))
abline(mPOC, col='red')

d = lm.codeRegressors(d,'Group', 'GroupReg')
mean(d$GroupReg)
d$cGroupReg = d$GroupReg - mean(d$GroupReg)

mWeighted = lm(DV ~ cGroupReg, data=d)
summary(mWeighted)
plot(jitter(d$cGroup, factor=.15), d$DV, xlim=c(-1,1))
abline(mWeighted, col='red')