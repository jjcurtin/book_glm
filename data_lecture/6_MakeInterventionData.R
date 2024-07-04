library(lmSupport)
library(truncnorm)
N=200   #sample size

InterventionGroup = c(rep(0,N/2),rep(1,N/2))
DepressBase = rtruncnorm(N,0,60,30,10)
B0 = 10
B1 = -4
B2 = 0.7
DepressPost = B0 + B1*InterventionGroup + B2*DepressBase + rnorm(N,0,8)
d = data.frame(InterventionGroup,DepressBase, DepressPost)
varDescribe(d)

m = lm(DepressPost ~ InterventionGroup + DepressBase, data=d)
modelSummary(m)
  

FileName = '6_TwoPredictors_Intervention.dat'
FilePath = 'G:/LectureDataR'
dfWriteDat(d,file.path(FilePath,FileName))
