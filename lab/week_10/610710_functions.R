# This file allows you to source in the many functions we have implemented 
# in lab.

# Sourcing is functionally very similar to installing a package with a few less
# perks.

#### varScore ####
varScore = function (Data, Forward, Reverse = NULL, Range = NULL, Prorate = TRUE, 
                     MaxMiss = 0.2) 
{
  d = Data[, c(Forward, Reverse)]
  if (!is.null(Range)) {
    if (min(d, na.rm = TRUE) < Range[1] || max(d, na.rm = TRUE) > 
        Range[2]) {
      stop("Item score(s) out of range")
    }
  }
  if (!is.null(Reverse) && length(Range) != 2) {
    stop("Must specify item range (Range) to reverse score items")
  }
  if (!is.null(Reverse)) {
    for (v in Reverse) {
      d[, v] = (Range[1] + Range[2]) - d[, v]
    }
  }
  if (Prorate) {
    Total = rowMeans(d, na.rm = TRUE) * dim(d)[2]
  }
  else {
    Total = rowSums(d, na.rm = TRUE)
  }
  MissCount = rowSums(is.na(d))
  MissCount = MissCount/dim(d)[2]
  Total[MissCount > MaxMiss] = NA
  return(Total)
}


#### gplotPredict ####
ggplotPredict = function (Model, Data = NULL, Label = NULL, Type = "response") 
{
  if (is.null(Data) & class(Model)[1] == "lm") {
    return(fitted.values(Model))
  }
  else {
    if (is.null(Label)) {
      PredictName = "Predicted"
      CILoName = "CILo"
      CIHiName = "CIHi"
      SEName = "SE"
    }
    else {
      PredictName = paste0("Predicted", Label)
      CILoName = paste0("CILo", Label)
      CIHiName = paste0("CIHi", Label)
      SEName = paste0("SE", Label)
    }
    Predictions = matrix(data = NA, nrow = nrow(Data), ncol = 4, 
                         dimnames = list(1:nrow(Data), c(PredictName, CILoName, 
                                                         CIHiName, SEName)))
    if (class(Model)[1] == "lm") {
      CILevel = 1 - 2 * pt(c(1), df = Model$df.residual, 
                           lower.tail = FALSE)
      Predictions[, 1:3] = predict(Model, newdata = Data, 
                                   interval = "confidence", level = CILevel)
      Predictions[, 4] = Predictions[, 1] - Predictions[, 
                                                        2]
      Predictions = as.data.frame(Predictions)
    }
    if (class(Model)[1] == "glm") {
      tmpPred = predict(Model, newdata = Data, type = "link", 
                        se.fit = TRUE)
      upr <- tmpPred$fit + tmpPred$se.fit
      lwr <- tmpPred$fit - tmpPred$se.fit
      fit <- tmpPred$fit
      if (Type == "response") {
        fit <- Model$family$linkinv(fit)
        upr <- Model$family$linkinv(upr)
        lwr <- Model$family$linkinv(lwr)
      }
      Predictions[, 1] = fit
      Predictions[, 2] = lwr
      Predictions[, 3] = upr
      Predictions[, 4] = Predictions[, 1] - Predictions[, 
                                                        2]
      Predictions = as.data.frame(Predictions)
    }
    if ((class(Model)[1] == "lmerMod") || (class(Model)[1] == 
                                           "glmerMod")) {
      Predictions[, c(1, 4)] = predictSE(Model, Data, 
                                         se.fit = TRUE, type = Type, level = 0, print.matrix = TRUE)
      Predictions[, 2] = Predictions[, 1] - Predictions[, 
                                                        4]
      Predictions[, 3] = Predictions[, 1] + Predictions[, 
                                                        4]
    }
    if (any(names(Data) == PredictName) || any(names(Data) == 
                                               CILoName) || any(names(Data) == CIHiName) || any(names(Data) == 
                                                                                                SEName)) {
      warning("Variable names (Predicted, CILo, CIHi, SE with Label PostFix) used in Data.  These variables removed before merging in predicted values")
      Data[, c(PredictName, CILoName, CIHiName, SEName)] = list(NULL)
    }
    Data = data.frame(Predictions, Data)
    return(Data)
  }
}

#### modelCaseAnalysis ####
modelCaseAnalysis = function (Model, Type = "RESIDUALS", Term = NULL, ID = row.names(Model$model)) 
{
  switch(toupper(Type), UNIVARIATE = {
    d = Model$model
    {
      Vars = names(d)
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      if (is.factor(d[[varname]])) {
        plot(d[varname], xlab = varname, ylab = "Frequency")
      } else {
        hist(d[[varname]], xlab = varname, main = "Red: Mean +- 3SD; Green: Median +- 2.2IQR")
        text(d[[varname]], rep(0, length(d[[varname]])), labels = ID, pos = 3, cex = 0.7)
        abline(v = c(-3, 0, 3) * sd(d[[varname]]) + 
                 mean(d[[varname]]), col = "red", lty = c(1, 
                                                          2, 1))
        abline(v = c(-2.2, 0, 2.2) * IQR(d[[varname]]) + 
                 median(d[[varname]]), col = "green", lty = c(1, 
                                                              2, 1))
      }
    }
  }, HATVALUES = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Small sample cut (green) = 3 * mean(Hat)\nLarge sample cut: 2 * mean(Hat)", 
                     sep = "")
    hist(hatvalues(Model), xlab = "Hat Values", main = TheTitle)
    abline(v = c(2, 3) * mean(hatvalues(Model)), col = c("red", 
                                                         "green"))
    text(hatvalues(Model), rep(0, length(hatvalues(Model))), labels = ID, pos = 3, cex = 0.7)
    points(hatvalues(Model), rep(0, length(hatvalues(Model))), 
           pch = "|", col = "blue")
  }, RESIDUALS = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    TheTitle = paste("Model: ", Model$call[2], "\n", "Bonferroni corrected p < .05 cut-off in red", 
                     sep = "")
    hist(rstudent(Model), xlab = "Studentized Residuals", 
         main = TheTitle)
    abline(v = c(-1, 1) * TCut, col = "red")
    text(rstudent(Model), rep(0, length(rstudent(Model))), labels = ID, pos = 3, cex = 0.7)
    points(rstudent(Model), rep(0, length(rstudent(Model))), 
           pch = "|", col = "blue")
  }, COOKSD = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    N = length(cooks.distance(Model))
    k = length(coef(Model)) - 1
    TheTitle = paste("Model: ", Model$call[2], "\n", "4/(N-P) cut-off (red)\nqf(.5,P,N-P) cut-off (green)", 
                     sep = "")
    hist(cooks.distance(Model), xlab = "Cooks d", main = TheTitle)
    abline(v = c((4/(N - k - 1)), qf(0.5, k + 1, N - k - 
                                       1)), col = c("red", "green"))
    text(cooks.distance(Model), rep(0, length(cooks.distance(Model))), labels = ID, pos = 3, cex = 0.7)
    points(cooks.distance(Model), rep(0, length(cooks.distance(Model))), 
           pch = "|", col = "blue")
  }, DFBETAS = {
    if (is.null(Term)) {
      {
        Vars = dimnames(dfbetas(Model))[[2]]
      }
    } else {
      if (!(Term %in% dimnames(dfbetas(Model))[[2]])) {
        stop("Term specified for DFBETAS not valid")
      } else Vars = Term
    }
    for (varname in Vars) {
      par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2, ask = TRUE)
      TheTitle = paste("Model: ", Model$call[2], "\n", 
                       "B= ", coef(Model)[varname], sep = "")
      hist(dfbetas(Model)[, varname], xlab = paste("DFBETAS:", 
                                                   varname), main = TheTitle)
      text(dfbetas(Model)[, varname], rep(0, length(dfbetas(Model)[, 
                                                                   varname])), labels = ID, pos = 3, cex = 0.7)
      abline(v = c(-2, 2), col = "red")
      points(dfbetas(Model)[, varname], rep(0, length(dfbetas(Model)[, varname])), 
             pch = "|", col = "blue")
    }
    
  }, INFLUENCEPLOT = {
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Influence Bubble plot", "\nModel: ", 
                     Model$call[2], sep = "")
    plot(hatvalues(Model), rstudent(Model), type = "n", 
         xlab = "Hat Values", ylab = "Studentized Residuals", 
         main = TheTitle)
    cooksize = 10 * sqrt(cooks.distance(Model))/max(cooks.distance(Model))
    points(hatvalues(Model), rstudent(Model), cex = cooksize)
    N = length(rstudent(Model))
    k = length(coef(Model)) - 1
    TCut <- qt(p = 0.025/N, df = N - k - 2, lower.tail = FALSE)
    abline(h = c(-1, 0, 1) * TCut, col = "red", lty = c(1, 
                                                        2, 1))
    abline(v = c(1, 2, 3) * mean(hatvalues(Model)), col = "red", 
           lty = c(2, 1, 1))
    text(hatvalues(Model),x = hatvalues(Model), y = rstudent(Model),
         rep(0, length(hatvalues(Model))), labels = ID, pos = 3, cex = 0.7)
  }, COVRATIO = {
    N = length(covratio(Model))
    k = length(coef(Model)) - 1
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    TheTitle = paste("Model: ", Model$call[2], "\n", "abs((3*P)/N)-1 cut-off in red", 
                     sep = "")
    hist(covratio(Model), xlab = "CovRatio", main = TheTitle)
    abline(v = abs((3 * (k + 1)/N) - 1), col = "red")
    text(covratio(Model), rep(0, length(covratio(Model))), labels = ID, pos = 3, cex = 0.7)
  }, {
    print("Valid options for type: hatvalues, residuals, cooksd, dfbetas, influenceplot, covratio, univariate")
  })
  Rownames = row.names(Model$model)
  Cases = list(Rownames = Rownames, Values = ID)
  return(Cases)
}

#### modelAssumptions ####
modelAssumptions = function (Model, Type = "NORMAL", ID = row.names(Model$model), 
                             one.page = TRUE) 
{
  switch(toupper(Type), NORMAL = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    qqPlot(Model, labels = FALSE, sim = TRUE, main = "Quantile-Comparison Plot to Assess Normality", 
           xlab = "t Quantiles", ylab = "Studentized Residuals")
    plot(density(rstudent(Model)), main = "Density Plot to Assess Normality of Residuals", 
         xlab = "Studentized Residual")
    zx <- seq(-4, 4, length.out = 100)
    lines(zx, dnorm(zx, mean = 0, sd = sd(rstudent(Model))), 
          lty = 2, col = "blue")
    cat("Descriptive Statistics for Studentized Residuals\n")
    describe(rstudent(Model))
  }, CONSTANT = {
    if (one.page) {
      dev.new(width = 14, height = 7)
      par(mfrow = c(1, 2))
    } else {
      dev.new(width = 7, height = 7, record = TRUE)
    }
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    plot(rstudent(Model) ~ fitted.values(Model), main = "Studentized Residuals vs. Fitted Values", 
         xlab = "Fitted Values", ylab = "Studentized Residuals")
    abline(h = 0, lty = 2, col = "blue")
    print(spreadLevelPlot(Model))
    cat("\n\n")
    print(ncvTest(Model))
  }, LINEAR = {
    dev.new(width = 7, height = 7, record = TRUE)
    par(cex.lab = 1.5, cex.axis = 1.2, lwd = 2)
    crPlots(Model, ask = TRUE)
  }, {
    print("Valid options for type: normal, constant, linear")
  })
}


#### modelBoxCox ####
modelBoxCox = function (Model, Lambdas = seq(-2, 2, by = 0.1)) 
{
  LR <- boxCox(Model, lambda = Lambdas)
  Lambda1Index <- sum(LR$x < 1)
  Chi1 <- mean(c(LR$y[Lambda1Index], LR$y[Lambda1Index + 1]))
  ChiLambda <- LR$y[which.max(LR$y)]
  ChiDiff <- 2 * (ChiLambda - Chi1)
  print(paste("Best Lambda=", round(LR$x[which.max(LR$y)], 
                                    2)))
  print(paste("Chi-square (df=1)=", round(ChiDiff, 2)))
  print(paste("p-value=", round(pchisq(ChiDiff, df = 1, lower.tail = FALSE), 
                                5)))
}
