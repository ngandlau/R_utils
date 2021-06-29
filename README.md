R\_utils
================

## Description

A library of R utility functions that I find useful for my work as a PhD
student on…

-   customer cohort analysis
-   age-period-cohort analysis
-   customer-based firm valuations
-   …

Includes utility functions that are useful for when one needs to work
with both Excel and R interchangeably.

## Generating Cohort Matrices

The library includes many function for creating cohort matrices from
scratch.

``` r
GenConstantValues(value = 10, dim = 5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   10   10   10   10   10
    ## [2,]   NA   10   10   10   10
    ## [3,]   NA   NA   10   10   10
    ## [4,]   NA   NA   NA   10   10
    ## [5,]   NA   NA   NA   NA   10

``` r
GenDiagonalValues(value = 10, dim = 5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   10    0    0    0    0
    ## [2,]    0   10    0    0    0
    ## [3,]    0    0   10    0    0
    ## [4,]    0    0    0   10    0
    ## [5,]    0    0    0    0   10

``` r
GenExpGrowthOverTime(factor = 0.9, dim = 5)
```

    ##      [,1] [,2] [,3]  [,4]   [,5]
    ## [1,]    1  0.9 0.81 0.729 0.6561
    ## [2,]   NA  1.0 0.90 0.810 0.7290
    ## [3,]   NA   NA 1.00 0.900 0.8100
    ## [4,]   NA   NA   NA 1.000 0.9000
    ## [5,]   NA   NA   NA    NA 1.0000

``` r
GenExpGrowthOverCohorts(factor = 1.1, dim = 5)
```

    ##      [,1] [,2] [,3]  [,4]   [,5]
    ## [1,]    1  0.0 0.00 0.000 0.0000
    ## [2,]    0  1.1 0.00 0.000 0.0000
    ## [3,]    0  0.0 1.21 0.000 0.0000
    ## [4,]    0  0.0 0.00 1.331 0.0000
    ## [5,]    0  0.0 0.00 0.000 1.4641

``` r
row <- c(0.9, 0.91, 0.92, 0.93, 0.94)
GenMatrixFromRow(row = row)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]  0.9 0.91 0.92 0.93 0.94
    ## [2,]   NA 0.90 0.91 0.92 0.93
    ## [3,]   NA   NA 0.90 0.91 0.92
    ## [4,]   NA   NA   NA 0.90 0.91
    ## [5,]   NA   NA   NA   NA 0.90

``` r
# From period by period-by-period retention rates to survival rates
retentionRates <- c(1, 0.9, 0.8, 0.7, 0.6)
retentionMatrix <- GenMatrixFromRow(row = retentionRates)
survivalRates <- TransformRetentionRates(
  X = retentionMatrix,
  from = "period-by-period",
  to = "survival"
)
survivalRates
```

    ##      [,1] [,2] [,3]  [,4]   [,5]
    ## [1,]    1  0.9 0.72 0.504 0.3024
    ## [2,]   NA  1.0 0.90 0.720 0.5040
    ## [3,]   NA   NA 1.00 0.900 0.7200
    ## [4,]   NA   NA   NA 1.000 0.9000
    ## [5,]   NA   NA   NA    NA 1.0000

``` r
numAcquiredCustomers <- c(100, 120, 140, 160, 180)
GenCustMatrixFromAcquisitions(
  nAcquired = numAcquiredCustomers,
  survMatrix = survivalRates
)
```

    ##      [,1] [,2] [,3] [,4]  [,5]
    ## [1,]  100   90   72 50.4 30.24
    ## [2,]   NA  120  108 86.4    NA
    ## [3,]   NA   NA  140   NA    NA
    ## [4,]   NA   NA   NA   NA    NA
    ## [5,]   NA   NA   NA   NA    NA

``` r
ageEffects <- c(10, 20, 30, 40, 50)
periodEffects <- c(0, 0, 0, 0, 5)
cohortEffects <- c(0, -2, -4, -6, -8)
intercept <- 0.1

# y_ijk = intercept + age_i + period_j + cohort_k
GenAPCMatrix(
  constant = intercept,
  ageEffects = ageEffects,
  periodEffects = periodEffects,
  cohortEffects = cohortEffects
)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,] 10.1 20.1 30.1 40.1 55.1
    ## [2,]   NA  8.1 18.1 28.1 43.1
    ## [3,]   NA   NA  6.1 16.1 31.1
    ## [4,]   NA   NA   NA  4.1 19.1
    ## [5,]   NA   NA   NA   NA  7.1
