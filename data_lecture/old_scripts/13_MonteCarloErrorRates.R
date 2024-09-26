# Setup --------------------------------------------------------------
rm(list=ls())   #clear all objects from workspace

library(lmSupport)
nExperiments = 10000
Type1 = rep(NA, nExperiments)  #set to 1 when any Type1 error observed, else 0
Type2 = rep(NA, nExperiments)  #set to 1 when any Type2 error observed, else 0


#3 Groups w/NO GROUP DIFFERENCES:  POCs-----------------------------------
#All POCs contrasts are focus (separate research questions)
M1 = 10
M2 = 10
M3 = 10

SD = 20
N=50

X = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X) = varContrasts(X, Type = 'POC', POCList = list(c(2,-1,-1), c(0, 1, -1)))  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)

for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m = lm(Y ~ X)
  results =summary(m)
  
  #if any of the two pairwise comparisons are significant
  if (any(c(results$coefficients[2,4], results$coefficients[3,4]) < .05))
  {
    Type1[i] = 1
  }
  else
  {
    Type1[i] = 0
  }
}
mean(Type1)

#3 Groups w/NO GROUP DIFFERENCES: 3 pairwise contrasts (no protection)------------------------------
M1 = 10
M2 = 10
M3 = 10

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)

for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  #if any of the three pairwise comparisons are significant after Holm adjustment
  if (any(c(results1$coefficients[2,4], results1$coefficients[3,4], results2$coefficients[2,4]) < .05))
  {
    Type1[i] = 1
  }
  else
  {
    Type1[i] = 0
  }
}
mean(Type1)


#3 Groups w/NO GROUP DIFFERENCES: FISHER LSD with 3 pairwise contrasts-----------------------------------
#
M1 = 10
M2 = 10
M3 = 10

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)


for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  #If onmibus test is signficant
  if (pf(results1$fstatistic[1], results1$fstatistic[2], results1$fstatistic[3], lower.tail=FALSE) < .05)
  {
    #if any of the three pairwise comparisons are significant
    if (results1$coefficients[2,4] < .05 || results1$coefficients[3,4] < .05 || results2$coefficients[2,4] < .05 )
    {
      Type1[i] = 1
    }
    else
    {
      Type1[i] = 0
    }
  }
  else
  {
    Type1[i] = 0
  }
}
mean(Type1)


###########################################3
#3 Groups w/NO GROUP DIFFERENCES:  Holm-Bonferroni only---------------------------------------------
M1 = 10
M2 = 10
M3 = 10

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)

for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  #if any of the three pairwise comparisons are significant after Holm adjustment
  if (any(p.adjust(c(results1$coefficients[2,4], results1$coefficients[3,4], results2$coefficients[2,4]), method='holm')< .05))
  {
    Type1[i] = 1
  }
  else
  {
    Type1[i] = 0
  }
}
mean(Type1)



#3 Groups w/ G3 different:  FISHER LSD ----------------------------
M1 = 10
M2 = 10
M3 = 20

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)


for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  #If onmibus test is signficant
  if (pf(results1$fstatistic[1], results1$fstatistic[2], results1$fstatistic[3], lower.tail=FALSE) < .05)
  {
    #if either of the two pairwise comparisons with G3 are not significant
    if (results1$coefficients[2,4] > .05 || results1$coefficients[3,4] > .05)
    {
      Type2[i] = 1  #failed to find a sig contrast
    }
    else
    {
      Type2[i] = 0  #found them both
    }
    
    #if G1 v G2 is signficant
    if (results2$coefficients[2,4] < .05)
    {
      Type1[i] = 1   #false alarm
    }
    else
    {
      Type1[i] = 0
    }    
    
  }
  else  #omnibus was not signficant
  {
    Type1[i] = 0  #no rejects so no Type1s
    Type2[i] = 1  #failed to find G3 different from G1 and G2
  }
}
mean(Type1)
mean(Type2)



#3 Groups w/G3 is different:  Holm-Bonferroni only-------------------------------------
M1 = 10
M2 = 10
M3 = 20

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)

for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  adjustedps = p.adjust(c(results1$coefficients[2,4], results1$coefficients[3,4], results2$coefficients[2,4]), method='holm')
  
  #if either of the contrasts with G3 are not signficant
  if (any(adjustedps[1:2] > .05))
  {
    Type2[i] = 1
  }
  else
  {
    Type2[i] = 0
  }
      
  #if G1vG2 is signficant
  if (adjustedps[3] < .05)
  {
    Type1[i] = 1
  }
  else
  {
    Type1[i] = 0
  }                
}
mean(Type1)
mean(Type2)

#3 Groups w/All Groups are different:  FISHER LSD-------------------------
M1 = 10
M2 = 20
M3 = 30

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)


for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  #If onmibus test is signficant
  if (pf(results1$fstatistic[1], results1$fstatistic[2], results1$fstatistic[3], lower.tail=FALSE) < .05)
  {
    #if any of pairwise comparisons with G3 are not significant
    if (results1$coefficients[2,4] > .05 || results1$coefficients[3,4] > .05 ||results2$coefficients[2,4] > .05 )
    {
      Type2[i] = 1  #failed to find a sig contrast
    }
    else
    {
      Type2[i] = 0  #found them all
    }
    
    
  }
  else  #omnibus was not signficant
  {
    Type2[i] = 1  #failed to find any groups different
  }
}
mean(Type1)
mean(Type2)

#3 Groups w/ All Groups  different:  Holm-Bonferroni only--------------------------------
M1 = 10
M2 = 20
M3 = 30

SD = 20
N=50

#NOTE:  It is NOT necessary to have different Xs (e.g., X1, X3 below) for your different contrasts.  I do it here to avoid having to reset contrasts inside the lopp which id very slow
X3 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X3) = lm.setContrasts(X3, RefLevel = 3)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)
X1 = factor(c(rep(1,N), rep(2,N), rep(3,N)))   #make an IV factor with 3 groups of N observations
contrasts(X1) = lm.setContrasts(X1, RefLevel = 1)  #set dummy contrasts with group 3 as control (to get group1 v group3 and group 2 vs group3)

for (i in 1:nExperiments)
{  
  Y = c(rnorm(N,M1,SD), rnorm(N,M2,SD),rnorm(N,M3,SD))  #Make a DV with M1 for group 1, M2 for group 2 and M3 for group 3.  SD for all groups
  
  m1 = lm(Y ~ X3)
  results1 =summary(m1)
  
  m2 = lm(Y~X1)
  results2 =summary(m2)
  
  adjustedps = p.adjust(c(results1$coefficients[2,4], results1$coefficients[3,4], results2$coefficients[2,4]), method='holm')
  
  #if any of the contrasts  are not signficant
  if (any(adjustedps > .05))
  {
    Type2[i] = 1
  }
  else
  {
    Type2[i] = 0
  }
                  
}
mean(Type1)
mean(Type2)
