#Make Data
# Sex = c(rep(0,20), rep(1,20))
# Rel = c(0,1)
# d = expand.grid(Sex=Sex, Rel=Rel)
# rY = c(rep(-3:3,2), 0,0,0,0,1,-1)
# d$Att[d$Sex==0 & d$Rel==0] = rY + 3
# d$Att[d$Sex==0 & d$Rel==1] = rY + 3
# d$Att[d$Sex==1 & d$Rel==0] = rY + 5
# d$Att[d$Sex==1 & d$Rel==1] = rY + 7
# dfWriteDat(d,'12_Interactions_Abortion.dat')


FilePath = 'G:/LectureDataR'
FileName = '12_Interactions_Abortion.dat'
d = dfReadDat(file.path(FilePath,FileName)

d$cSex = varRecode(d$Sex, c(0,1), c(-.5,.5))
d$cRel = varRecode(d$Rel, c(0,1), c(-.5,.5))

mcAdd = lm(Att ~ cSex + cRel, data=d)
modelSummary(mcAdd)
dNew = data.frame(cSex = c(-.5,-.5,.5,.5), cRel = c(-.5,.5,-.5,.5))
dNew = modelPredictions(mcAdd,dNew)



#Bar plots
options (device='windows')
library(gplots)   #for barplot2

#Religion as focal variable
Means = matrix(c(3,3,5,7), nrow=2,ncol=2, byrow=TRUE, dimnames= list(c('Male', 'Female'), c('Catholic', 'Jewish')))
BarColors = c('red', 'green')
windows()
barplot2(t(Means), beside = TRUE, ylim = c(0,8), xlab = 'Sex', ylab = 'Attitudes about Abortion', plot.ci =FALSE, axes=TRUE, col= BarColors, cex.names=1.25, font=2, lwd=2)
#mtext('Attitudes about Abortion', side=2, line=3, cex=1.5, font=2)
#mtext('Sex', side=1, line=3,cex=1.5, font=2)
legend('topleft',rownames(t(Means)), fill=BarColors,cex=1.25,box.lwd=3, inset=.02)

#Sex as focal variable
Means = matrix(c(3,3,5,7), nrow=2,ncol=2, byrow=TRUE, dimnames= list(c('Male', 'Female'), c('Catholic', 'Jewish')))
BarColors = c('red', 'green')
windows()
barplot2(Means, beside = TRUE, ylim = c(0,8), xlab = 'Religion', ylab = 'Attitudes about Abortion', plot.ci =FALSE, axes=TRUE, col= BarColors, cex.names=1.25, cex.lab=1.5, font=2, lwd=2)
#mtext('Attitudes about Abortion', side=2, line=3, cex=1.5, font=2)
#mtext('Religion', side=1, line=3,cex=1.5, font=2)
legend('topleft',rownames(Means), fill=BarColors,cex=1.25,box.lwd=3, inset=.02)


########################################################

#Make centered line plot (Additive)
Means = matrix(c(2.5,3.5,5.5,6.5), nrow=2,ncol=2, byrow=TRUE, dimnames= list(c('Male', 'Female'), c('Catholic', 'Jewish')))
windows()
plot(c(-.5, .5), c(1,8), type='n', axes = FALSE, xlab='', ylab='')
lines(x=c(-.5, .5), y=Means[,1], col='red', type='o', lwd=2, cex=1.25)
lines(x=c(-.5, .5), y=Means[,2], col='green', type='o', lwd=2, cex=1.25)
axis(2, line=1, lwd=2, cex=1.25, font=2)
mtext('Attitudes about Abortion', side=2, line=3, cex=1.5, font=2)
axis(1, at=  c(-.5, 0, .5), line=1, lwd=2, cex=1.25, font=2)
mtext('Gender', side=1, line=4,cex=1.5, font=2)
mtext('Female', side=1, at = 0.5,line=3,cex=1.5, font=2)
mtext('Male', side=1, at = -.5,line=3,cex=1.5, font=2)
legend('topleft',c('Catholic (-.5)', 'Jewish (.5)'), fill=c('red', 'green'),cex=1.25,box.lwd=3, inset=.02)

#Make centered line plot (interactive)
Means = matrix(c(3,3,5,7), nrow=2,ncol=2, byrow=TRUE, dimnames= list(c('Male', 'Female'), c('Catholic', 'Jewish')))
windows()
plot(c(-.5, .5), c(0,8), type='n', axes = FALSE, xlab='', ylab='')
lines(x=c(-.5, .5), y=Means[,1], col='red', type='o', lwd=2, cex=1.25)
lines(x=c(-.5, .5), y=Means[,2], col='green', type='o', lwd=2, cex=1.25)
axis(2, line=1, lwd=2, cex=1.25, font=2)
mtext('Attitudes about Abortion', side=2, line=3, cex=1.5, font=2)
axis(1, at=  c(-.5, 0, .5), line=1, lwd=2, cex=1.25, font=2)
mtext('Gender', side=1, line=4,cex=1.5, font=2)
mtext('Female', side=1, at = 0.5,line=3,cex=1.5, font=2)
mtext('Male', side=1, at = -.5,line=3,cex=1.5, font=2)
legend('topleft',c('Catholic (-.5)', 'Jewish (.5)'), fill=c('red', 'green'),cex=1.25,box.lwd=3, inset=.02)


#Make dummy line plot (interactive)
Means = matrix(c(3,3,5,7), nrow=2,ncol=2, byrow=TRUE, dimnames= list(c('Male', 'Female'), c('Catholic', 'Jewish')))
windows()
plot(c(0, 1), c(1,8), type= 'n', axes = FALSE, xlab='', ylab='')
lines(x=c(0, 1), y=Means[,1], col='red', type='o', lwd=2, cex=1.25)
lines(x=c(0, 1), y=Means[,2], col='green', type='o', lwd=2, cex=1.25)
axis(2, line=1, lwd=2, cex=1.25, font=2)
mtext('Attitudes about Abortion', side=2, line=3, cex=1.5, font=2)
axis(1, at=  c(0, 0.5, 1), line=1, lwd=2, cex=1.25, font=2)
mtext('Gender', side=1, line=4,cex=1.5, font=2)
mtext('Female', side=1, at = 1,line=3,cex=1.5, font=2)
mtext('Male', side=1, at = 0,line=3,cex=1.5, font=2)
legend('topleft',c('Catholic (0)', 'Jewish (1)'), fill=c('red', 'green'),cex=1.25,box.lwd=3, inset=.02)