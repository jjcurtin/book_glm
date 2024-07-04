Like0 = rnorm(10000,mean = 0, sd=24 )
min(Like0)
max(Like0)
Like0 = Like0 - mean(Like0)
mean(Like0)
lm.describeData(Like0)

windows()
hist(Like0)

d = data.frame(SubID = seq(1,length(Like0)), Like0)
setwd("C:/Users/LocalUser/Desktop/GLM")
lm.writeDat(d, '3_SamplingDistributions_Like.dat')