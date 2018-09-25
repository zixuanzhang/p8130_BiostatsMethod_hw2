HW2\_code
================

Problem 6
=========

A study is to examine the associations of depression and cognitive performance with migraine symptoms:

Some backgrounds:
-----------------

CESD: questionairre screening for depression; meausre symptoms of depression; sum of scores to all questions; longer duration of symptoms will have greater score.

nddie: The Neurological Disorders Depression Inventory for Epilepsy (NDDI-E) is a 6-item questionnaire validated to screen for depression in people with epilepsy

Import data
-----------

``` r
library(readxl)

excel_sheets("Migraine.xlsx")
```

    ## [1] "Sheet1"

``` r
Mig <- read_excel("Migraine.xlsx")

Mig <- janitor::clean_names(Mig) # rename variable names
```

Have a look at the data:

``` r
head(Mig)
```

    ## # A tibble: 6 x 5
    ##   migraine  cesd nddie abnas_memory abnas_language
    ##      <dbl> <dbl> <dbl>        <dbl>          <dbl>
    ## 1        0    42    NA            1              0
    ## 2        0    NA     6            0              0
    ## 3        0    10    11            0              0
    ## 4        0    NA    NA            1              0
    ## 5        0    23    16            1              0
    ## 6        0     6     8            1              0

``` r
str(Mig)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    419 obs. of  5 variables:
    ##  $ migraine      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ cesd          : num  42 NA 10 NA 23 6 NA NA NA NA ...
    ##  $ nddie         : num  NA 6 11 NA 16 8 NA NA NA NA ...
    ##  $ abnas_memory  : num  1 0 0 1 1 1 0 2 0 4 ...
    ##  $ abnas_language: num  0 0 0 0 0 0 0 0 0 0 ...

variables in the data:

| Variable names | description                                          |
|:--------------:|:-----------------------------------------------------|
|    Migraine    | Migraine status: 0(no), 1(yes)                       |
|      CESD      | Depression Scale                                     |
|      NDDIE     | Self rating tool to screen for depression, inventory |
|  ABNAS memory  | Assessement for memory                               |
| ABNAS language | Assessment for language                              |

exploratory analysis
--------------------

Seperate subjects with and without migraine.

``` r
with_Mig <- filter(Mig, migraine == 1)
no_Mig <- filter(Mig, migraine == 0)
```

Explore subjects with migraine

``` r
summary(with_Mig$cesd)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    6.00   11.00   14.41   20.00   46.00       8

``` r
summary(no_Mig$cesd)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    3.00    8.00   10.68   14.00   48.00      63

plots:

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

summary table for patients with migraine:

<table>
<colgroup>
<col width="7%" />
<col width="12%" />
<col width="10%" />
<col width="12%" />
<col width="10%" />
<col width="12%" />
<col width="10%" />
<col width="12%" />
<col width="12%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>with_mig_cesd</th>
<th>no_mig_cesd</th>
<th>with_mig_nddie</th>
<th>no_mig_nddie</th>
<th>with_mig_memory</th>
<th>no_mig_memory</th>
<th>with_mig_language</th>
<th>no_mig_languge</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>mean</td>
<td></td>
</tr>
<tr class="even">
<td>sd</td>
<td></td>
</tr>
<tr class="odd">
<td>median</td>
<td></td>
</tr>
</tbody>
</table>
