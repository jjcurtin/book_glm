####### R code for Unit 2: Sampling Distributions ###################
#Last edited on 2017-09-07, JJC


####### Load packages ###############################################
library(lmSupport)


####### Ocean Liking Example ########################################
#open and view data file
FilePath = 'G:/LectureDataR'
FileName = '2_SamplingDistributions_Like.dat'
d = dfReadDat(file.path(FilePath, FileName))  

str(d)   #get basic info about the data frame
head(d)  #view the first 6 values
some(d)  #view a random 10 values
View(d)  #view data in a spreadsheet

#Descriptives and histogram of Like0 scores
varDescribe(d) #calculate common descriptives for variables in data frame

#Set up window for graphics
windows()  #Make an external window for plotting on PC;  MAC users use quartz()
par('cex' = 1.5, 'lwd' = 2, 'font.axis'=1.5, 'font.lab' = 2) #set parameters for pretty graphs
hist(d$Like0, xlim= c(-100,100), col = 'yellow') #histogram with yellow bars and specified range of x-axis lables


#get 1000 random samples of Like0 scores of N=10.  Save each in a column of new dataframe called dS
nSamples = 1000
nParticipants = 10
dS = matrix(NA,nrow=nParticipants, ncol=nSamples, dimnames = list((1:nParticipants), paste(rep('Sample',nSamples),1:nSamples,sep=''))) #make matrix to save 20 samples of 10

for (i in 1:nSamples)   
{
  dS[,i] = sample(d$Like0,nParticipants)     #loop to get the 1000 samples of 10
}

dS = data.frame(dS) #convert to data frame

t=varDescribe(dS,1)  #table of descriptives (n, mean, sd, min, max) and save in object t.  1 reports minimal stats.  For more stats use 2 or 3
t[c(1:20, 999:1000),]  #display some on screen
varDescribe(t[2],1) #descriptives for the mean of each sample
hist(as.matrix(t[2]),  xlim = c(-100,100), main= 'Histogram of Sample Means', xlab= 'Like0 Sample Means', col='red') #histogram of sample means


#######  Make dataset to demonstrate central limit theoreom #########
dDemo = matrix(NA,nrow=100000, ncol=3, dimnames = list((1:100000), c('normal', 'uniform', 'skewed')))
dDemo[,1] = rnorm(100000, 100, 10)         #100,000 scores from normal dist with mean =100, sd=10
dDemo[,2] = runif(100000, min=60, max=140) #100,000 scores from uniform distribution with min=60, max =140
dDemo[,3] = rnorm(100000, 100,10)
dDemo[,3] = dDemo[,3] - min(dDemo[,3])
dDemo[,3] = dDemo[,3] ^4      #100,000 scores from positive skewed distribution (4 steps)
dDemo = data.frame(dDemo)

#figs for normal distribution population
par('cex' = 1.5, 'lwd' = 2, 'font.axis'=2, 'font.lab' = 2) #set parameters for pretty graphs
M= round(mean(dDemo$normal),1)
SD= round(sd(dDemo$normal),1)
SKEW= round(skew(dDemo$normal),1)
KUR = round(kurtosi(dDemo$normal),1)
hist(dDemo$normal, xlim = c(60,140), main=paste('Normal Population \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='yellow')

dSamp = matrix(NA,nrow=5,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$normal,5)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=5 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=10,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$normal,10)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=10 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=20,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$normal,20)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=20 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=50,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$normal,50)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=50 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=100,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$normal,100)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=100 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')


#figs for uniform distribution population
par('cex' = 1.5, 'lwd' = 2, 'font.axis'=2, 'font.lab' = 2) #set parameters for pretty graphs
M= round(mean(dDemo$uniform),1)
SD= round(sd(dDemo$uniform),1)
SKEW= round(skew(dDemo$uniform),1)
KUR = round(kurtosi(dDemo$uniform),1)
hist(dDemo$uniform, xlim = c(60,140), main=paste('Uniform Population \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='yellow')

dSamp = matrix(NA,nrow=5,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$uniform,5)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=5 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=10,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$uniform,10)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=10 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=20,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$uniform,20)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=20 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=50,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$uniform,50)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=50 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=100,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$uniform,100)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(60,140), main=paste('Sampling Distribution, N=100 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

#figs for skewed distribution population
par('cex' = 1.5, 'lwd' = 2, 'font.axis'=2, 'font.lab' = 2) #set parameters for pretty graphs
M= round(mean(dDemo$skewed),1)
SD= round(sd(dDemo$skewed),1)
SKEW= round(skew(dDemo$skewed),1)
KUR = round(kurtosi(dDemo$skewed),1)
hist(dDemo$skewed, main=paste('Skewed Population \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='yellow')

dSamp = matrix(NA,nrow=5,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$skewed,5)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(0,3e+07), main=paste('Sampling Distribution, N=5 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=10,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$skewed,10)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(0,3e+07), main=paste('Sampling Distribution, N=10 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=20,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$skewed,20)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(0,3e+07), main=paste('Sampling Distribution, N=20 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=50,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$skewed,50)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(0,3e+07), main=paste('Sampling Distribution, N=50 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')

dSamp = matrix(NA,nrow=100,ncol=10000)
for (i in 1:10000) dSamp[,i] = sample(dDemo$skewed,100)
dMeans = as.matrix(describe(dSamp)[3])
M= round(mean(dMeans),1)
SD= round(sd(dMeans),1)
SKEW= round(skew(dMeans),1)
KUR = round(kurtosi(dMeans),1)
hist(dMeans, xlim = c(0,3e+07), main=paste('Sampling Distribution, N=100 \nM=',M, 'SD=', SD, '\nSkew=', SKEW, 'Kurtosis= ',KUR), xlab = '', col='red')



###### Figures for Testing parameter estimate from extended example #####

hist(as.matrix(t[2]),  xlim = c(-50,50), main= 'Histogram of Sample Means', xlab= 'Like0 Sample Means', col='red') #histogram with breaks every 3
hist(as.matrix(t[2]),  xlim = c(-50,50), freq= FALSE, main= 'Histogram of Sample Means', xlab= 'Like0 Sample Means', col='red') #histogram with breaks every 3
lines(x=seq(-50,50,.01), dnorm(seq(-50,50,.01), 0, sd(d$Like0)/sqrt(10)), lwd=3, col="blue")
abline(v=2.4, lwd=3, col='green')

plot(x=seq(-6.7,6.7,.001),y=dnorm(seq(-6.7,6.7,.001),0,1), xlim= c(-6,6),type='l', col= 'blue', xlab= 'Z', ylab= 'Probability')
abline(v=0.32, lwd=3, col='green')
abline(v=-0.32, lwd=3, col='green')
pnorm(0.32, mean=0, sd=1, lower.tail=FALSE)  * 2


plot(x=seq(-6.7,6.7,.001),y=dnorm(seq(-6.7,6.7,.001),0,1), xlim= c(-6,6),type='l', col= 'blue', xlab= 'z or t score', ylab= 'Probability')
lines(x=seq(-6.7,6.7,.001),y=dt(seq(-6.7,6.7,.001), df=9), col= 'red')
legend('topright', c('z', 't'), lwd=2, col = c('blue', 'red'))


plot(x=seq(-3,3,.001),y=dnorm(seq(-3,3,.001),0,1), xlim= c(-3,3),type='l', col= 'blue', xlab= 'z or t score', ylab= 'Probability')
lines(x=seq(-3,3,.001),y=dt(seq(-3,3,.001), df=1), col= 'red')
lines(x=seq(-3,3,.001),y=dt(seq(-3,3,.001), df=10), col= 'green')
lines(x=seq(-3,3,.001),y=dt(seq(-3,3,.001), df=100), col= 'yellow')
legend('topright', c('z', 't(df=1)', 't(df=10)', 't(df=100)'), lwd=2, col = c('blue', 'red', 'green', 'yellow'))