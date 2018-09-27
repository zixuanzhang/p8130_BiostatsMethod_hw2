Analysis for HEP Data (Problem 6)
================
Eleanor Zhang
2018-09-27

Overview
========

This document aims to analyze a research study which examining association of depression and cognitive performance with migraine symtoms. HEP Data were collected from patients with new onset of focal epilepsy with their informations including migraine status, score of NDDIE (Neurological Disorders Depression Inventory for Epilepsy), score of CESD (a questionairre screening for depression), cognitive evaluation for both memory and language.

Import Data and Data Cleaning
=============================

``` r
Mig <- read_excel("Migraine.xlsx")
Mig <- janitor::clean_names(Mig) # rename variable names
Mig$migraine_status <- ifelse(Mig$migraine == 1, "have_migraine", "no_migraine")
```

Exploratory Analysis
====================

### Part I: descriptive statistics without cutoffs

Firstly, we can have an general look at the data:

``` r
head(Mig)
```

    ## # A tibble: 6 x 6
    ##   migraine  cesd nddie abnas_memory abnas_language migraine_status
    ##      <dbl> <dbl> <dbl>        <dbl>          <dbl> <chr>          
    ## 1        0    42    NA            1              0 no_migraine    
    ## 2        0    NA     6            0              0 no_migraine    
    ## 3        0    10    11            0              0 no_migraine    
    ## 4        0    NA    NA            1              0 no_migraine    
    ## 5        0    23    16            1              0 no_migraine    
    ## 6        0     6     8            1              0 no_migraine

``` r
str(Mig)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    419 obs. of  6 variables:
    ##  $ migraine       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cesd           : num  42 NA 10 NA 23 6 NA NA NA NA ...
    ##  $ nddie          : num  NA 6 11 NA 16 8 NA NA NA NA ...
    ##  $ abnas_memory   : num  1 0 0 1 1 1 0 2 0 4 ...
    ##  $ abnas_language : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ migraine_status: chr  "no_migraine" "no_migraine" "no_migraine" "no_migraine" ...

``` r
skimr::skim(Mig)
```

    ## Skim summary statistics
    ##  n obs: 419 
    ##  n variables: 6 
    ## 
    ## ── Variable type:character ─────────────────────────────────────────────────────────────────────────────────────
    ##         variable missing complete   n min max empty n_unique
    ##  migraine_status       0      419 419  11  13     0        2
    ## 
    ## ── Variable type:numeric ───────────────────────────────────────────────────────────────────────────────────────
    ##        variable missing complete   n  mean    sd p0 p25 p50  p75 p100
    ##  abnas_language       0      419 419  1.68  2.19  0   0   1  2.5    9
    ##    abnas_memory       0      419 419  2.7   3.12  0   0   2  4     12
    ##            cesd      71      348 419 11.47 10.65  0   3   9 16     48
    ##        migraine       0      419 419  0.2   0.4   0   0   0  0      1
    ##           nddie      73      346 419 10.53  4.48  6   7   9 13     24
    ##      hist
    ##  ▇▂▁▁▁▁▁▁
    ##  ▇▃▁▂▁▁▁▁
    ##  ▇▆▂▂▁▁▁▁
    ##  ▇▁▁▁▁▁▁▂
    ##  ▇▃▂▂▁▁▁▁

Our data is consisted of 419 observations with 5 variables. Some variables contains missing values which need to be taken care of.

Since we are interested in the relationship of cognitive performance, with or without migraine, and depression conditions, we can look at and compare the subset of each variable corresponding to migraine status. So here we subset the whole dataset into two subsets:

``` r
with_Mig <- filter(Mig, migraine == 1)
str(with_Mig)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    82 obs. of  6 variables:
    ##  $ migraine       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ cesd           : num  2 27 7 6 2 16 2 1 NA 2 ...
    ##  $ nddie          : num  8 14 6 13 6 11 6 8 NA 7 ...
    ##  $ abnas_memory   : num  0 2 0 2 0 0 0 1 0 1 ...
    ##  $ abnas_language : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ migraine_status: chr  "have_migraine" "have_migraine" "have_migraine" "have_migraine" ...

``` r
no_Mig <- filter(Mig, migraine == 0)
str(no_Mig)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    337 obs. of  6 variables:
    ##  $ migraine       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cesd           : num  42 NA 10 NA 23 6 NA NA NA NA ...
    ##  $ nddie          : num  NA 6 11 NA 16 8 NA NA NA NA ...
    ##  $ abnas_memory   : num  1 0 0 1 1 1 0 2 0 4 ...
    ##  $ abnas_language : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ migraine_status: chr  "no_migraine" "no_migraine" "no_migraine" "no_migraine" ...

Since the `skim` function allows us to the general trend of each variable, we would expect the variables are significantly skewed. To better describe the data, we will use *median* and *interquartile range* to describe each variable in these two sets **with\_Mig** and **no\_Mig**.

``` r
#cesd
summary(no_Mig$cesd)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    3.00    8.00   10.68   14.00   48.00      63

``` r
summary(with_Mig$cesd)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    6.00   11.00   14.41   20.00   46.00       8

``` r
#nddie
summary(no_Mig$nddie)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    6.00    6.00    9.00   10.29   13.00   24.00      64

``` r
summary(with_Mig$nddie)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    6.00    8.00   11.00   11.42   14.00   23.00       9

``` r
#abnas_memory
summary(no_Mig$abnas_memory)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   2.000   2.555   4.000  12.000

``` r
summary(with_Mig$abnas_memory)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   1.000   2.000   3.305   5.000  12.000

``` r
#abnas_language
summary(no_Mig$abnas_language)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   1.000   1.602   2.000   9.000

``` r
summary(with_Mig$abnas_language)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   1.000   1.976   3.000   8.000

To describe the dataset in terms of sample size and missing values:

|                                    |   cesd  |  nddie  | abnas\_memory | abnas\_language |
|------------------------------------|:-------:|:-------:|:-------------:|:---------------:|
| no migraine (sample size\[NAs\])   | 337(63) | 337(64) |     337(0)    |      337(0)     |
| with migraine (sample size\[NAs\]) |  82(8)  |  82(9)  |     82(0)     |      82(0)      |

Put all information about median and IQR together as a table:

|                                     |   cesd   |   nddie  | abnas\_memory | abnas\_language |
|-------------------------------------|:--------:|:--------:|:-------------:|:---------------:|
| no migraine (median\[IQR range\])   |  8(3,14) |  9(6,13) |     2(0,4)    |      1(0,2)     |
| with migraine (median\[IQR range\]) | 11(6,20) | 11(8,14) |     2(1,5)    |      1(0,3)     |

### part II: descriptive statistics with cutoffs

Now we want to further subset some variabls by cutoff values:

-   nddie: &gt;= 16
-   cesd: &gt;= 16

Look at proportions of each variables in terms of with migraine or without migrain

``` r
ggplot(Mig, aes(x = migraine_status, y = cesd, fill = migraine_status)) + 
  geom_boxplot()
```

    ## Warning: Removed 71 rows containing non-finite values (stat_boxplot).

![](HW2_code_files/figure-markdown_github/unnamed-chunk-3-1.png)

#### histograms:

``` r
par(mfrow = c(1, 2))
hist(no_Mig$cesd, freq = F, ylim = c(0, 0.1))
hist(with_Mig$cesd, freq = F, ylim = c(0, 0.1))
```

![](HW2_code_files/figure-markdown_github/cesd-1.png)

``` r
par(mfrow = c(1, 2))
hist(no_Mig$nddie, freq = F, ylim = c(0, 0.3))
hist(with_Mig$nddie, freq = F, ylim = c(0, 0.3))
```

![](HW2_code_files/figure-markdown_github/nddie-1.png)

``` r
par(mfrow = c(2, 2))
hist(no_Mig$abnas_memory, freq = F, ylim = c(0, 0.6))
hist(with_Mig$abnas_memory, freq = F, ylim = c(0, 0.6))
hist(no_Mig$abnas_language, freq = F, ylim = c(0, 0.7))
hist(with_Mig$abnas_language, freq = F, ylim = c(0, 0.7))
```

![](HW2_code_files/figure-markdown_github/abnas-1.png)

#### boxplots

``` r
Mig$migraine_status <- ifelse(Mig$migraine == 1, "have_migraine", "no_migraine")
ggplot(Mig, aes(x = migraine, y = cesd, fill = migraine_status) ) + geom_boxplot()
```

    ## Warning: Removed 71 rows containing non-finite values (stat_boxplot).

![](HW2_code_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(Mig, aes(x = migraine, y = nddie, fill = migraine_status)) + geom_boxplot()
```

    ## Warning: Removed 73 rows containing non-finite values (stat_boxplot).

![](HW2_code_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
ggplot(Mig, aes(x = migraine, y = abnas_memory, fill = migraine_status)) + geom_boxplot()
```

![](HW2_code_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
ggplot(Mig, aes(x = migraine, y = abnas_language, fill = migraine_status)) + geom_boxplot()
```

![](HW2_code_files/figure-markdown_github/unnamed-chunk-4-4.png)
