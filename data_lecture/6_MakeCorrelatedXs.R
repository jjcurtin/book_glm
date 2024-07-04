
N=200   #sample size

#Start with two standard normal Xs with 0 correlation
MuX = c(0,0)  #means for X1 and X2
SDX = c(1,1)  #variances
SigmaX = matrix(rep(0,length(MuX)*length(MuX)),nrow=length(MuX),ncol=length(MuX),dimnames = list(c('X1','X2'),c('X1','X2')))
diag(SigmaX) = SDX

#View Sigma (Var-covar matrix)
SigmaX

#Now set off diag values of  = rX1X2
SigmaX[2,1] = .5
SigmaX[1,2] = .5

#View Sigma to confirm
SigmaX

#make Xs with Mu and Sigma
d = data.frame(mvrnorm(N,MuX,SigmaX, empirical = TRUE))

#Now give Xs the mean and SD you want
MeanX1 =10
SDX1 = 2
MeanX2 = 100
SDX2 = 15
d$X1 = d$X1*SDX1 + MeanX1
d$X2 = d$X2*SDX2 + MeanX2



#Verify
varDescribe(d)
cor(d)

B0=-70
B1 = 3
B2 = 1
d$Y = B0 + B1*d$X1 + B2*d$X2+ rnorm(N,0,30)
varDescribe(d)

m = lm(Y ~ X1 + X2, data=d)
modelSummary(m)
varDescribe(d)
d$Y = d$Y/1.5 +10

FileName = '6_TwoPredictors_Exam.dat'
FilePath = 'G:/LectureDataR'
dfWriteDat(d,file.path(FilePath,FileName))
