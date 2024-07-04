library(lmSupport)

#Open data
FilePath = 'G:/LectureDataR'
FileName = '5_SingleDichotomous_BG_FPS.dat'
d=dfReadDat(file.path(FilePath,FileName))

#Explore data
str(d)
head(d)
varDescribe(d)
table(d$BG2)
varDescribeBy(d$FPS,d$BG2)

#estimate 2 parameter model
mBG2 = lm(FPS ~ BG2, data=d)
modelSummary(mBG2)

#plot 2 parameter model
windows()
plot(d$BG2,d$FPS)
abline(mBG2)

#compact model to test B1
m1 = lm(FPS ~ 1, data=d)
modelSummary(m1)
modelCompare(m1,mBG2)

#compact model to test B0
m1a = lm(FPS ~ -1 + BG2, data=d)
modelSummary(m1a)
modelCompare(m1a,mBG2)

#Recode BG2 with zero-centered coefficients
d$BG2
d$BG2zc = varRecode(d$BG2, c(0,1), c(-.5, .5))
d$BG2zc  #check it worked

#Estimate and display 2 parameter model with zero-centered coefficients
#for BG2 model
mBG2zc = lm(FPS ~ BG2zc, data=d)
modelSummary(mBG2zc)

windows()
plot(d$BG2zc,d$FPS)
abline(mBG2zc)

#Estimate and display 2 parameter model with mean-centered coefficients
#for BG2 model
d$BG2mc = d$BG2 - mean(d$BG2) #mean center
d$BG2mc
mBG2mc = lm(FPS ~ BG2mc, data=d)
modelSummary(mBG2mc)

windows()
plot(d$BG2mc,d$FPS)
abline(mBG2mc)

#Estimate and display 2 parameter model with -1, 1 coefficients
#for BG2 model
d$BG2o = varRecode(d$BG2, c(0,1), c(-1,1))#mean center
d$BG2o
mBG2o = lm(FPS ~ BG2o, data=d)
modelSummary(mBG2o)

windows()
plot(d$BG2o,d$FPS)
abline(mBG2o)


#Pub quality graph with lmSupport and base package 

pY = data.frame(BG2=c(0,1))
pY = modelPredictions(mBG2, pY)
Means = matrix(pY$Predicted)
rownames(Means) = c('No-alcohol', 'Alcohol')
SE = pY$SE

bars.col = c('white', 'gray')
bars.density = c(-1,-1)  #negative density suppresses lines
bars.angle = c(0,0,0)

figNewDevice()
figBarPlot(Means,ylim=c(-100,200), lab.text='Group', se=SE, bars.col=bars.col,bars.density=bars.density, bars.angle = bars.angle)
figAxis(side=2,lab.text=expression(bold(paste('Fear Potentiated Startle (', mu, 'V)', sep=''))), scale.at=seq(-100,200,by=50))
figLegend('topleft', legend=rownames(Means),fill=bars.col, angle=bars.angle, density=bars.density)
figPoints(jitter(rep(1.5,24), amount=.02),d$FPS[d$BG2==0])
figPoints(jitter(rep(3.5,72), amount=.02),d$FPS[d$BG2==1])
