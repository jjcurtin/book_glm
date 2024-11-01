rm(list=ls())   #clear all objects from workspace

library(lmSupport)
FilePath = 'G:/LectureDataR'

#Between subjects
FileName = '14_BloodDopeLong.dat'
dB = dfReadDat(file.path(FilePath,FileName), SubID=NULL)  #ignore SubID b/c it repeats (for within subject demo)
dB$Cond = factor(dB$Cond, levels = c('Con', 'Txt'))
contrasts(dB$Cond) = varContrasts(dB$Cond,Type='POC', POCList = list(c(-1,1)), Labels = 'Txt_v_Con')
mB = lm(Time ~ Cond, data=dB)
modelSummary(mB)


#Within subjects 1
FileName = '14_BloodDopeWide.dat'
dW = dfReadDat(file.path(FilePath,FileName))

dW$Diff = dW$TimeCon-dW$TimeTxt
dW$Avg = (dW$TimeCon+dW$TimeTxt)/2

mDiff = lm(Diff ~ 1, data=dW)
modelSummary(mDiff)

mAvg = lm(Avg ~ 1, data=dW)
modelSummary(mAvg)

#Within subjects 2
FileName = '14_BloodDopeWide2.dat'
dW2 = dfReadDat(file.path(FilePath,FileName))

dW2$Diff = dW2$TimeCon-dW2$TimeTxt
dW2$Avg = (dW2$TimeCon+dW2$TimeTxt)/2

mDiff2 = lm(Diff ~ 1, data=dW2)
modelSummary(mDiff2)


#Within Subject with Between IV
FileName = '14_BloodDopeWideGroup.dat'
dW3 = dfReadDat(file.path(FilePath,FileName))
dW3$Group = factor(dW3$Group, levels = c('Recreation', 'College'))
contrasts(dW3$Group) = varContrasts(dW3$Group, Type='POC', POCList = list(c(1,-1)))

mGroup = lm(Diff~Group, data=dW3)
