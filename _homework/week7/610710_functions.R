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