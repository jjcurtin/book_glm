# This file allows you to source in the many functions we have implemented 
# in lab.

# Sourcing is functionally very similar to installing a package with a few less
# perks.

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