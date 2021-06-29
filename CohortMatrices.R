GenConstantValues <- function(value, dim){
  matrix <- t(sapply(1:dim, function(t) {
    zerosBefore <- rep(NA, t-1)
    values <- rep(value, dim)
    row <- c(zerosBefore, values)
    rowTrimmed <- row[1:dim]
    rowTrimmed
  }))
  return(matrix)
}

GenDiagonalValues <- function(value, dim) {
  return(diag(value, ncol = dim, nrow = dim))
}

GenExpGrowthOverTime <- function(factor, dim) {
  values <- sapply(1:dim, function(t) factor^(t-1))
  matrix <- t(sapply(1:dim, function(t){
    row <- c(rep(NA, t-1), values)
    rowTrimmed <- row[1:dim]
  }))
  return(matrix)
}

GenExpGrowthOverCohorts <- function(factor, dim) {
  matrixEntries <- unlist(sapply(1:dim, function(t) {
    row <- c(factor^(t-1), rep(0, dim))
  }))
  matrix <- t(matrix(
    data = matrixEntries,
    ncol = dim,
    nrow = dim
  ))
  return(matrix)
}

GenDiscountMatrix <- function(discRate, dim, startFromPeriod = 1) {
  row <- sapply(1:(dim - startFromPeriod + 1), function(t) {
    1 / ((1+discRate)^(t-1))
  })
  row <- c(rep(0, startFromPeriod-1), row)
  matrix <- matrix(
    data = rep(row, dim), 
    ncol = dim,
    byrow = T
  )
  return(matrix)
}

GenAPCMatrix <- function(constant, ageEffects, periodEffects, cohortEffects){
  dim <- length(ageEffects)
  Xperiod <- matrix(
    data = rep(periodEffects, dim),
    nrow = dim,
    ncol = dim,
    byrow = T
  )
  Xcohort <- matrix(
    data = rep(cohortEffects, dim),
    nrow = dim,
    ncol = dim
  )
  Xperiodcohort <- Xperiod + Xcohort
  Xperiodcohort <- TransformView(Xperiodcohort, from = "cohort-period", to ="cohort-age")
  
  Xage <- matrix(
    data = rep(ageEffects, dim),
    nrow = dim,
    ncol = dim,
    byrow = T
  )
  Xageperiodcohort <- Xage + Xperiodcohort
  Xageperiodcohort <- TransformView(Xageperiodcohort, from = "cohort-age", to = "cohort-period")
  return(Xageperiodcohort + constant)
}

# Each cohort develops according to the process described by "row"
GenMatrixFromRow <- function(row){
  X <- t(sapply(0:(length(row)-1), function(i){
    if (i == 0) return(row)
    row[(length(row)-i+1):length(row)] <- 0
    return(row)
  }))
  X <- TransformView(X, from = "cohort-age", to = "cohort-period")
  return(X)
}

# Generates a customer matrix given you have 
#   1) number of acquired customers
#   2) survival matrix
GenCustMatrixFromAcquisitions <- function(nAcquired, survMatrix){
  row <- nAcquired
  nAcquiredMatrix <- sapply(0:(length(nAcquired)-1), function(i){
    if (i == 0) return(row)
    row[(length(row)-i+1):length(row)] <- NA
    return(row)
  })
  return(nAcquiredMatrix * survMatrix)
}

# Converts a cohort-period with monthly cohorts/periods to a 
# cohort-period matrix with yearly cohorts/periods
MonthlyToYearlyCohortMatrix <- function(Xmonth, aggFunc = "sum"){
  nMonths <- nrow(Xmonth)
  nYears <- nMonths %/% 12
  
  if (aggFunc == "sum"){
    Xyears <- t(sapply(1:nYears, function(cohort){
      # from [1, 13, 25, ...]
      # to   [12, 24, 36. ...]
      rowStart <- 12*cohort - 11
      rowEnd <- 12*cohort
      currCohortCells <- sapply(1:nYears, function(t){
        colStart <- 12*t - 11
        colEnd <- 12*t
        cell <- sum(Xmonth[rowStart:rowEnd, colStart:colEnd], na.rm = T)
        return(cell)
      })
    }))
  }
  return(Xyears)
}

#' Computes the period-by-period net dollar retention rate given a cohort-period
#' matrix.
#'
#' @param revenueMatrix NxN cohort-period matrix
#'
#' @return vector of num
AggNetDollarRetention <- function(revenueMatrix){
  sapply(2:ncol(revenueMatrix), function(t){
    revenueBefore <- sum(revenueMatrix[1:(t-1), (t-1)], na.rm=T)
    revenueAfter  <- sum(revenueMatrix[1:(t-1), t], na.rm=T)
    netDollarRetention <- revenueAfter / revenueBefore
    return(netDollarRetention)
  })
}

#' Transforms cohort matrices into different formats
#'
#' @param X: cohort matrix
#' @param from: one of "cohort-period" or "cohort-age"
#' @param to: one of "cohort-period" or "cohort-age"
#'
#' @return matrix
TransformView <- function(X, from, to){
  if (from == "cohort-period" & to == "cohort-age"){
    Xout <- sapply(1:nrow(X), function(rowIdx){
      row <- X[rowIdx, ]
      rowTransf <- row[rowIdx:ncol(X)]
      rowTransf <- FillUpNAs(rowTransf, ncol(X))
    })
    return(t(Xout))
  } else if (from == "cohort-age" & to == "cohort-period"){
    dim <- ncol(X)
    Xout <- sapply(1:nrow(X), function(rowIdx){
      row <- X[rowIdx, 1:(dim-rowIdx+1)]
      if (length(row) != dim){
        row <- c(rep(NA, rowIdx-1), row)
      }
      return(row)
    })
  }
  return(t(Xout))
}

# Transforms period-by-period retention rates to survival probabilities
# only works for cohort-period array
TransformRetentionRates <- function(X, from = "period-by-period", to = "survival"){
  if (from == "period-by-period" & to == "survival"){
    dim <- ncol(X)
    Xout <- sapply(1:nrow(X), function(rowIdx){
      row <- X[rowIdx, rowIdx:dim]
      row <- cumprod(row)
      if (all(is.na(diag(X)))){
        row <- c(1, row)
        row <- row[1:(length(row)-1)]
      } 
      if (length(row) != dim){
        row <- c(rep(NA, rowIdx-1), row)
      }
      return(row)
    })
  }
  return(t(Xout))
}

Subset <- function(matrix, n){
  return(matrix[1:n, 1:n])
}

#' Plot cohort table without DT package
#'
#' @param X matrix: a cohort matrix
#' @param rowSums bool: if T adds rowsum to the bottom of the matrix
#' @param colSums bool: if T adds colsum to the right of the matrix
#' @param to int: cut table at `to` cohorts/periods
#' @param view string: either of c("cohort-period", "cohort-age")
#'
#' @return
#' @export
#'
#' @examples
PlotCohortTable1 <- function(X, rowSums, colSums, to, view){
  Xs <- X[1:to, 1:to]
  if (view == "cohort-period"){
    Xs <- AddRowColSums(Xs, X, rowSums = rowSums, colSums = colSums)
    Xs <- Replace(Xs, oldValue = 0, newValue = NA)
    if (colSums == T){
      rownames(Xs) <- c(sapply(1:to, function(c) paste0("c=", c)), "SUM")
    } else {
      rownames(Xs) <- sapply(1:to, function(c) paste0("c=", c))
    }
    df <- data.frame(Xs)
    if (rowSums == T){
      names(df) <- c(sapply(1:to, function(t) paste0("t=", t)), "Infinite SUM")
    } else {
      names(df) <- sapply(1:to, function(t) paste0("t=", t))
    }
  } else if (view == "cohort-age"){
    Xs <- TransformView(Xs, from = "cohort-period", to = "cohort-age")
    Xs <- AddRowColSums(Xs, X, rowSums = rowSums, colSums = colSums)
    Xs <- Replace(Xs, oldValue = 0, newValue = NA)
    if (colSums == T){
      rownames(Xs) <- c(sapply(1:to, function(c) paste0("c=", c)), "SUM")
    } else {
      rownames(Xs) <- sapply(1:to, function(c) paste0("c=", c))
    }
    df <- data.frame(Xs)
    if (rowSums == T){
      names(df) <- c(sapply(1:to, function(t) paste0("a=", t-1)), "Infinite SUM")
    } else {
      names(df) <- sapply(1:to, function(t) paste0("a=", t-1))
    }
  }
  return(df)
}

#' Adds row/column sums to a cohort matrix.
#'
#' @param X: matrix; 
#' @param Xs: matrix; subset of X, e.g. X[1:10, 1:10]
#' @return matrix
AddRowColSums <- function(Xs, X, rowSums, colSums) {
  rowSum <- rowSums(X, na.rm = T)  # infinite time horizon
  colSum <- colSums(Xs, na.rm = T)
  to <- ncol(Xs)
  
  if (rowSums == T & colSums == T){
    Xs <- cbind(Xs, rowSum[1:to])
    Xs <- rbind(Xs, c(colSum[1:to], colSum[length(colSum)]))
  }
  
  if (rowSums == T & colSums == F){
    Xs <- cbind(Xs, rowSum[1:to])
  }
  
  if (rowSums == F & colSums == T){
    Xs <- rbind(Xs, colSums(Xs, na.rm = T))
  }
  return(Xs)
}

# Replaces values in a matrix
Replace <- function(X, oldValue, newValue){
  if (!is.na(oldValue)){
    X[X == oldValue] <- newValue
  } else {
    X[is.na(oldValue)] <- newValue
  }
  return(X)
}

# Appends NAs to the right of the vector until length(x)==length
FillUpNAs <- function(x, length){
  if (length == length(x)) return(x)
  return(c(x, rep(NA, length - length(x))))
}
