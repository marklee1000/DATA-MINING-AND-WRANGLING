SA2
================
BAYBAYON, DARLYN ANTOINETTE; MAYOL, JOSE RAPHAEL J.
2025-05-18

# 1. **Dataset Familiarization & Preparation**

``` r
set.seed(123)
```

## Libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggcorrplot)
```

    ## Warning: package 'ggcorrplot' was built under R version 4.4.3

``` r
library(patchwork)
```

    ## Warning: package 'patchwork' was built under R version 4.4.3

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.4.3

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-8

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.4.3

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.4.3

    ## Type 'citation("pROC")' for a citation.
    ## 
    ## Attaching package: 'pROC'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
library(rpart)
```

    ## Warning: package 'rpart' was built under R version 4.4.3

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 4.4.3

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.4.3

    ## randomForest 4.7-1.2
    ## Type rfNews() to see new features/changes/bug fixes.
    ## 
    ## Attaching package: 'randomForest'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(ranger)
```

    ## Warning: package 'ranger' was built under R version 4.4.3

    ## 
    ## Attaching package: 'ranger'
    ## 
    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

``` r
library(xgboost)
```

    ## Warning: package 'xgboost' was built under R version 4.4.3

    ## 
    ## Attaching package: 'xgboost'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     slice

``` r
library(Matrix)
```

## Training Set

``` r
df_train <- read.csv("churn-bigml-80.csv")
head(df_train)
```

    ##   State Account.length Area.code International.plan Voice.mail.plan
    ## 1    KS            128       415                 No             Yes
    ## 2    OH            107       415                 No             Yes
    ## 3    NJ            137       415                 No              No
    ## 4    OH             84       408                Yes              No
    ## 5    OK             75       415                Yes              No
    ## 6    AL            118       510                Yes              No
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                    25             265.1             110            45.07
    ## 2                    26             161.6             123            27.47
    ## 3                     0             243.4             114            41.38
    ## 4                     0             299.4              71            50.90
    ## 5                     0             166.7             113            28.34
    ## 6                     0             223.4              98            37.98
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             197.4              99            16.78               244.7
    ## 2             195.5             103            16.62               254.4
    ## 3             121.2             110            10.30               162.6
    ## 4              61.9              88             5.26               196.9
    ## 5             148.3             122            12.61               186.9
    ## 6             220.6             101            18.75               203.9
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                91              11.01               10.0                3
    ## 2               103              11.45               13.7                3
    ## 3               104               7.32               12.2                5
    ## 4                89               8.86                6.6                7
    ## 5               121               8.41               10.1                3
    ## 6               118               9.18                6.3                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.70                      1 False
    ## 2              3.70                      1 False
    ## 3              3.29                      0 False
    ## 4              1.78                      2 False
    ## 5              2.73                      3 False
    ## 6              1.70                      0 False

## Testing Set

``` r
df_test <- read.csv("churn-bigml-20.csv")
head(df_test)
```

    ##   State Account.length Area.code International.plan Voice.mail.plan
    ## 1    LA            117       408                 No              No
    ## 2    IN             65       415                 No              No
    ## 3    NY            161       415                 No              No
    ## 4    SC            111       415                 No              No
    ## 5    HI             49       510                 No              No
    ## 6    AK             36       408                 No             Yes
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                     0             184.5              97            31.37
    ## 2                     0             129.1             137            21.95
    ## 3                     0             332.9              67            56.59
    ## 4                     0             110.4             103            18.77
    ## 5                     0             119.3             117            20.28
    ## 6                    30             146.3             128            24.87
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             351.6              80            29.89               215.8
    ## 2             228.5              83            19.42               208.8
    ## 3             317.8              97            27.01               160.6
    ## 4             137.3             102            11.67               189.6
    ## 5             215.1             109            18.28               178.7
    ## 6             162.5              80            13.81               129.3
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                90               9.71                8.7                4
    ## 2               111               9.40               12.7                6
    ## 3               128               7.23                5.4                9
    ## 4               105               8.53                7.7                6
    ## 5                90               8.04               11.1                1
    ## 6               109               5.82               14.5                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.35                      1 False
    ## 2              3.43                      4  True
    ## 3              1.46                      4  True
    ## 4              2.08                      2 False
    ## 5              3.00                      1 False
    ## 6              3.92                      0 False

## Drop non-predictive identifiers

``` r
df_train <- subset(df_train, select = -c(State))
head(df_train)
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1            128       415                 No             Yes
    ## 2            107       415                 No             Yes
    ## 3            137       415                 No              No
    ## 4             84       408                Yes              No
    ## 5             75       415                Yes              No
    ## 6            118       510                Yes              No
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                    25             265.1             110            45.07
    ## 2                    26             161.6             123            27.47
    ## 3                     0             243.4             114            41.38
    ## 4                     0             299.4              71            50.90
    ## 5                     0             166.7             113            28.34
    ## 6                     0             223.4              98            37.98
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             197.4              99            16.78               244.7
    ## 2             195.5             103            16.62               254.4
    ## 3             121.2             110            10.30               162.6
    ## 4              61.9              88             5.26               196.9
    ## 5             148.3             122            12.61               186.9
    ## 6             220.6             101            18.75               203.9
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                91              11.01               10.0                3
    ## 2               103              11.45               13.7                3
    ## 3               104               7.32               12.2                5
    ## 4                89               8.86                6.6                7
    ## 5               121               8.41               10.1                3
    ## 6               118               9.18                6.3                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.70                      1 False
    ## 2              3.70                      1 False
    ## 3              3.29                      0 False
    ## 4              1.78                      2 False
    ## 5              2.73                      3 False
    ## 6              1.70                      0 False

``` r
df_test <- subset(df_test, select = -c(State))
head(df_test)
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1            117       408                 No              No
    ## 2             65       415                 No              No
    ## 3            161       415                 No              No
    ## 4            111       415                 No              No
    ## 5             49       510                 No              No
    ## 6             36       408                 No             Yes
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                     0             184.5              97            31.37
    ## 2                     0             129.1             137            21.95
    ## 3                     0             332.9              67            56.59
    ## 4                     0             110.4             103            18.77
    ## 5                     0             119.3             117            20.28
    ## 6                    30             146.3             128            24.87
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             351.6              80            29.89               215.8
    ## 2             228.5              83            19.42               208.8
    ## 3             317.8              97            27.01               160.6
    ## 4             137.3             102            11.67               189.6
    ## 5             215.1             109            18.28               178.7
    ## 6             162.5              80            13.81               129.3
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                90               9.71                8.7                4
    ## 2               111               9.40               12.7                6
    ## 3               128               7.23                5.4                9
    ## 4               105               8.53                7.7                6
    ## 5                90               8.04               11.1                1
    ## 6               109               5.82               14.5                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.35                      1 False
    ## 2              3.43                      4  True
    ## 3              1.46                      4  True
    ## 4              2.08                      2 False
    ## 5              3.00                      1 False
    ## 6              3.92                      0 False

## Check for NA values

``` r
df_train %>%
  summarise_all(~sum(is.na(.)))
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1              0         0                  0               0
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                     0                 0               0                0
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1                 0               0                0                   0
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                 0                  0                  0                0
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1                 0                      0     0

``` r
df_test %>%
  summarise_all(~sum(is.na(.)))
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1              0         0                  0               0
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                     0                 0               0                0
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1                 0               0                0                   0
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                 0                  0                  0                0
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1                 0                      0     0

## Convert the True/False & Yes/No columns into binary

``` r
df_train <- df_train %>% 
  mutate(Churn = ifelse(Churn == "False", 0, 1))
head(df_train)
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1            128       415                 No             Yes
    ## 2            107       415                 No             Yes
    ## 3            137       415                 No              No
    ## 4             84       408                Yes              No
    ## 5             75       415                Yes              No
    ## 6            118       510                Yes              No
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                    25             265.1             110            45.07
    ## 2                    26             161.6             123            27.47
    ## 3                     0             243.4             114            41.38
    ## 4                     0             299.4              71            50.90
    ## 5                     0             166.7             113            28.34
    ## 6                     0             223.4              98            37.98
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             197.4              99            16.78               244.7
    ## 2             195.5             103            16.62               254.4
    ## 3             121.2             110            10.30               162.6
    ## 4              61.9              88             5.26               196.9
    ## 5             148.3             122            12.61               186.9
    ## 6             220.6             101            18.75               203.9
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                91              11.01               10.0                3
    ## 2               103              11.45               13.7                3
    ## 3               104               7.32               12.2                5
    ## 4                89               8.86                6.6                7
    ## 5               121               8.41               10.1                3
    ## 6               118               9.18                6.3                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.70                      1     0
    ## 2              3.70                      1     0
    ## 3              3.29                      0     0
    ## 4              1.78                      2     0
    ## 5              2.73                      3     0
    ## 6              1.70                      0     0

``` r
df_test <- df_test %>% 
  mutate(Churn = ifelse(Churn == "False", 0, 1))
head(df_test)
```

    ##   Account.length Area.code International.plan Voice.mail.plan
    ## 1            117       408                 No              No
    ## 2             65       415                 No              No
    ## 3            161       415                 No              No
    ## 4            111       415                 No              No
    ## 5             49       510                 No              No
    ## 6             36       408                 No             Yes
    ##   Number.vmail.messages Total.day.minutes Total.day.calls Total.day.charge
    ## 1                     0             184.5              97            31.37
    ## 2                     0             129.1             137            21.95
    ## 3                     0             332.9              67            56.59
    ## 4                     0             110.4             103            18.77
    ## 5                     0             119.3             117            20.28
    ## 6                    30             146.3             128            24.87
    ##   Total.eve.minutes Total.eve.calls Total.eve.charge Total.night.minutes
    ## 1             351.6              80            29.89               215.8
    ## 2             228.5              83            19.42               208.8
    ## 3             317.8              97            27.01               160.6
    ## 4             137.3             102            11.67               189.6
    ## 5             215.1             109            18.28               178.7
    ## 6             162.5              80            13.81               129.3
    ##   Total.night.calls Total.night.charge Total.intl.minutes Total.intl.calls
    ## 1                90               9.71                8.7                4
    ## 2               111               9.40               12.7                6
    ## 3               128               7.23                5.4                9
    ## 4               105               8.53                7.7                6
    ## 5                90               8.04               11.1                1
    ## 6               109               5.82               14.5                6
    ##   Total.intl.charge Customer.service.calls Churn
    ## 1              2.35                      1     0
    ## 2              3.43                      4     1
    ## 3              1.46                      4     1
    ## 4              2.08                      2     0
    ## 5              3.00                      1     0
    ## 6              3.92                      0     0

## Handle categorical variables via **one-hot encoding**.

``` r
df_train <- df_train %>%
  mutate(across(where(is.character), as.factor)) %>%
  pivot_longer(cols = where(is.factor), names_to = "variable", values_to = "value") %>%
  unite("var_value", variable, value, sep = "_") %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = var_value, values_from = value, values_fill = 0)
head(df_train)
```

    ## # A tibble: 6 × 21
    ##   Account.length Area.code Number.vmail.messages Total.day.minutes
    ##            <int>     <int>                 <int>             <dbl>
    ## 1            128       415                    25              265.
    ## 2            107       415                    26              162.
    ## 3            137       415                     0              243.
    ## 4             84       408                     0              299.
    ## 5             75       415                     0              167.
    ## 6            118       510                     0              223.
    ## # ℹ 17 more variables: Total.day.calls <int>, Total.day.charge <dbl>,
    ## #   Total.eve.minutes <dbl>, Total.eve.calls <int>, Total.eve.charge <dbl>,
    ## #   Total.night.minutes <dbl>, Total.night.calls <int>,
    ## #   Total.night.charge <dbl>, Total.intl.minutes <dbl>, Total.intl.calls <int>,
    ## #   Total.intl.charge <dbl>, Customer.service.calls <int>, Churn <dbl>,
    ## #   International.plan_No <dbl>, Voice.mail.plan_Yes <dbl>,
    ## #   Voice.mail.plan_No <dbl>, International.plan_Yes <dbl>

``` r
df_test <- df_test %>%
  mutate(across(where(is.character), as.factor)) %>%
  pivot_longer(cols = where(is.factor), names_to = "variable", values_to = "value") %>%
  unite("var_value", variable, value, sep = "_") %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = var_value, values_from = value, values_fill = 0)
head(df_test)
```

    ## # A tibble: 6 × 21
    ##   Account.length Area.code Number.vmail.messages Total.day.minutes
    ##            <int>     <int>                 <int>             <dbl>
    ## 1            117       408                     0              184.
    ## 2             65       415                     0              129.
    ## 3            161       415                     0              333.
    ## 4            111       415                     0              110.
    ## 5             49       510                     0              119.
    ## 6             36       408                    30              146.
    ## # ℹ 17 more variables: Total.day.calls <int>, Total.day.charge <dbl>,
    ## #   Total.eve.minutes <dbl>, Total.eve.calls <int>, Total.eve.charge <dbl>,
    ## #   Total.night.minutes <dbl>, Total.night.calls <int>,
    ## #   Total.night.charge <dbl>, Total.intl.minutes <dbl>, Total.intl.calls <int>,
    ## #   Total.intl.charge <dbl>, Customer.service.calls <int>, Churn <dbl>,
    ## #   International.plan_No <dbl>, Voice.mail.plan_No <dbl>,
    ## #   Voice.mail.plan_Yes <dbl>, International.plan_Yes <dbl>

## Convert Area.code to factor

``` r
df_train$Area.code <- as.factor(df_train$Area.code)
df_test$Area.code <- as.factor(df_test$Area.code)
```

# 2. Exploratory Data Analysis

## Churn Rate (Train)

``` r
df_train %>%
  summarise(ChurnRate = sum(Churn == 1) / n())
```

    ## # A tibble: 1 × 1
    ##   ChurnRate
    ##       <dbl>
    ## 1     0.146

## Churn Rate (Test)

``` r
df_test %>%
  summarise(ChurnRate = sum(Churn == 1) / n())
```

    ## # A tibble: 1 × 1
    ##   ChurnRate
    ##       <dbl>
    ## 1     0.142

## Distribution of call minutes - Day

``` r
ggplot(df_train, aes(x = Total.day.minutes)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 binwidth = 10) +
  geom_density(lwd = 1, color = "#00AFBB",
               fill = "#00AFBB", alpha = 0.25) +
  ggtitle("Distribution for Call Minutes During the Day") +
  theme_minimal()
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Distribution of call minutes - Evening

``` r
ggplot(df_train, aes(x = Total.eve.minutes)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 binwidth = 10) +
  geom_density(lwd = 1, color = "#00AFBB",
               fill = "#00AFBB", alpha = 0.25) +
  ggtitle("Distribution for Call Minutes During the Evening") +
  theme_minimal()
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Distribution of call minutes - Night

``` r
ggplot(df_train, aes(x = Total.night.minutes)) + 
  geom_histogram(aes(y = ..density..),
                 colour = 1, fill = "white",
                 binwidth = 10) +
  geom_density(lwd = 1, color = "#00AFBB",
               fill = "#00AFBB", alpha = 0.25) +
  ggtitle("Distribution for Call Minutes During the Night") +
  theme_minimal()
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

## Service Usage

``` r
ggplot(df_train,aes(x = factor(Area.code))) + 
  geom_bar(fill = "#00AFBB") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  ggtitle("Service Usage by Area") + 
  xlab("Area Code") +
  ylab("Count") + 
  theme_minimal()
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

## Check for **correlation** among numerical variables.

### Training Set

``` r
numeric_data_train <- df_train %>% 
  select(-International.plan_No, -Voice.mail.plan_Yes, -Voice.mail.plan_No, -International.plan_Yes) %>%
  select(where(is.numeric))
corr_matrix_train <- cor(numeric_data_train, use = "complete.obs")

ggcorrplot(corr_matrix_train,
           method = "square",
           lab = TRUE,
           lab_size = 1.5,
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix",
           ggtheme = ggplot2::theme_minimal())
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

### Testing Set

``` r
numeric_data_test <- df_test %>% 
  select(-International.plan_No, -Voice.mail.plan_Yes, -Voice.mail.plan_No, -International.plan_Yes) %>%
  select(where(is.numeric))
corr_matrix_test <- cor(numeric_data_test, use = "complete.obs")

ggcorrplot(corr_matrix_test,
           method = "square",
           lab = TRUE,
           lab_size = 1.5,
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix",
           ggtheme = ggplot2::theme_minimal())
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

There is a problem surrounding the relationship between charges and
minutes for both sets since the correlation values are always 1. This
will eventually do the model disservice, hence we must remove one of
them. Charge seems to be the more plausible choice since minutes do
better work in conveying important business information.

``` r
df_train <- subset(df_train, select = -c(Total.day.charge,Total.eve.charge,Total.night.charge,Total.intl.charge))
head(df_train)
```

    ## # A tibble: 6 × 17
    ##   Account.length Area.code Number.vmail.messages Total.day.minutes
    ##            <int> <fct>                     <int>             <dbl>
    ## 1            128 415                          25              265.
    ## 2            107 415                          26              162.
    ## 3            137 415                           0              243.
    ## 4             84 408                           0              299.
    ## 5             75 415                           0              167.
    ## 6            118 510                           0              223.
    ## # ℹ 13 more variables: Total.day.calls <int>, Total.eve.minutes <dbl>,
    ## #   Total.eve.calls <int>, Total.night.minutes <dbl>, Total.night.calls <int>,
    ## #   Total.intl.minutes <dbl>, Total.intl.calls <int>,
    ## #   Customer.service.calls <int>, Churn <dbl>, International.plan_No <dbl>,
    ## #   Voice.mail.plan_Yes <dbl>, Voice.mail.plan_No <dbl>,
    ## #   International.plan_Yes <dbl>

``` r
df_test <- subset(df_test, select = -c(Total.day.charge,Total.eve.charge,Total.night.charge,Total.intl.charge))
head(df_test)
```

    ## # A tibble: 6 × 17
    ##   Account.length Area.code Number.vmail.messages Total.day.minutes
    ##            <int> <fct>                     <int>             <dbl>
    ## 1            117 408                           0              184.
    ## 2             65 415                           0              129.
    ## 3            161 415                           0              333.
    ## 4            111 415                           0              110.
    ## 5             49 510                           0              119.
    ## 6             36 408                          30              146.
    ## # ℹ 13 more variables: Total.day.calls <int>, Total.eve.minutes <dbl>,
    ## #   Total.eve.calls <int>, Total.night.minutes <dbl>, Total.night.calls <int>,
    ## #   Total.intl.minutes <dbl>, Total.intl.calls <int>,
    ## #   Customer.service.calls <int>, Churn <dbl>, International.plan_No <dbl>,
    ## #   Voice.mail.plan_No <dbl>, Voice.mail.plan_Yes <dbl>,
    ## #   International.plan_Yes <dbl>

## Examine **class imbalance** and report if applicable.

Create a copy to get the non-one-hot encoded versions.

``` r
df_train_1 <- read.csv("churn-bigml-80.csv")
df_test_1 <- read.csv("churn-bigml-20.csv")
```

### Training Test

``` r
p1_train <- ggplot(df_train_1, aes(x = `International.plan`)) +
  geom_bar(fill = "forestgreen") +
  theme_minimal() +
  ggtitle("International Plan")

p2_train <- ggplot(df_train_1, aes(x = `Voice.mail.plan`)) +
  geom_bar(fill = "darkorange") +
  theme_minimal() +
  ggtitle("Voice Mail Plan")

p3_train <- ggplot(df_train_1, aes(x = Churn)) +
  geom_bar(fill = "firebrick") +
  theme_minimal() +
  ggtitle("Churn")

print( p1_train / p2_train / p3_train +
  plot_layout(ncol = 1))
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### Testing Set

``` r
p1_test <- ggplot(df_test_1, aes(x = `International.plan`)) +
  geom_bar(fill = "forestgreen") +
  theme_minimal() +
  ggtitle("International Plan")

p2_test <- ggplot(df_test_1, aes(x = `Voice.mail.plan`)) +
  geom_bar(fill = "darkorange") +
  theme_minimal() +
  ggtitle("Voice Mail Plan")

p3_test <- ggplot(df_test_1, aes(x = Churn)) +
  geom_bar(fill = "firebrick") +
  theme_minimal() +
  ggtitle("Churn")

print( p1_test / p2_test / p3_test +
  plot_layout(ncol = 1))
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

It can be clearly observed that all three classes are NOT balanced, with
the True values being much less than their counterparts.

## Dataset Balancing

``` r
table(df_train$Churn)
```

    ## 
    ##    0    1 
    ## 2278  388

``` r
class_weights <- ifelse(df_train$Churn == 1, 2278 / 388, 1)
```

# 3. Modeling and Comparison

## Regression-based Models

### Logistic Regression (Unweighted)

``` r
log_model <- glm(Churn ~ ., data = df_train, family = "binomial")
summary(log_model)
```

    ## 
    ## Call:
    ## glm(formula = Churn ~ ., family = "binomial", data = df_train)
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -5.8901591  0.8042564  -7.324 2.41e-13 ***
    ## Account.length          0.0008571  0.0015685   0.546  0.58476    
    ## Area.code415           -0.0042765  0.1540792  -0.028  0.97786    
    ## Area.code510           -0.0620970  0.1772761  -0.350  0.72613    
    ## Number.vmail.messages   0.0374140  0.0206237   1.814  0.06966 .  
    ## Total.day.minutes       0.0125970  0.0012189  10.335  < 2e-16 ***
    ## Total.day.calls         0.0028906  0.0030978   0.933  0.35076    
    ## Total.eve.minutes       0.0056747  0.0012705   4.467 7.95e-06 ***
    ## Total.eve.calls        -0.0007921  0.0030830  -0.257  0.79724    
    ## Total.night.minutes     0.0028341  0.0012418   2.282  0.02248 *  
    ## Total.night.calls       0.0020081  0.0031686   0.634  0.52624    
    ## Total.intl.minutes      0.1000653  0.0227826   4.392 1.12e-05 ***
    ## Total.intl.calls       -0.1201463  0.0288257  -4.168 3.07e-05 ***
    ## Customer.service.calls  0.5077385  0.0440836  11.518  < 2e-16 ***
    ## International.plan_No  -2.0977592  0.1595603 -13.147  < 2e-16 ***
    ## Voice.mail.plan_Yes    -2.0315469  0.6553953  -3.100  0.00194 ** 
    ## Voice.mail.plan_No             NA         NA      NA       NA    
    ## International.plan_Yes         NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2212.2  on 2665  degrees of freedom
    ## Residual deviance: 1730.6  on 2650  degrees of freedom
    ## AIC: 1762.6
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
df_test_log <- df_test
df_test_log$predicted_prob <- predict(log_model, newdata = df_test_log, type = "response")
df_test_log$predicted_class <- ifelse(df_test_log$predicted_prob > 0.45, 1, 0)

conf_log <- confusionMatrix(factor(df_test_log$predicted_class), factor(df_test_log$Churn))
conf_log
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 548  76
    ##          1  24  19
    ##                                           
    ##                Accuracy : 0.8501          
    ##                  95% CI : (0.8207, 0.8763)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : 0.7316          
    ##                                           
    ##                   Kappa : 0.2048          
    ##                                           
    ##  Mcnemar's Test P-Value : 3.397e-07       
    ##                                           
    ##             Sensitivity : 0.9580          
    ##             Specificity : 0.2000          
    ##          Pos Pred Value : 0.8782          
    ##          Neg Pred Value : 0.4419          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.8216          
    ##    Detection Prevalence : 0.9355          
    ##       Balanced Accuracy : 0.5790          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score <- function(precision, recall) {
  precision <- as.numeric(precision)
  recall <- as.numeric(recall)
  return(2 * (precision * recall) / (precision + recall))
}
f1_score(conf_log$byClass["Pos Pred Value"], conf_log$byClass["Sensitivity"])
```

    ## [1] 0.916388

``` r
quick_roc_auc <- function(model, x_test, y_test, positive_class = 1) {
  probs <- predict(model, newdata = x_test, type = "response")
  y_test <- factor(y_test, levels = c(1 - positive_class, positive_class))
  roc_obj <- roc(y_test, probs)
  plot(roc_obj, col = "blue", lwd = 2, main = paste("AUC =", round(auc(roc_obj), 3)))
  return(auc(roc_obj))
}
```

``` r
quick_roc_auc(log_model, df_test_log, df_test_log$Churn)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

    ## Area under the curve: 0.8261

**Performance Metrics:**

- **Accuracy:** 85.01%

- **Precision:** 87.82%

- **Recall:** 95.80%

- **F1-score:** 91.6388%

- **AUC:** 82.6%

### Logistic Regression (Weighted)

``` r
log_model_w <- glm(Churn ~ ., data = df_train, family = "binomial", weights = class_weights)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` r
summary(log_model_w)
```

    ## 
    ## Call:
    ## glm(formula = Churn ~ ., family = "binomial", data = df_train, 
    ##     weights = class_weights)
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                          Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -3.7375767  0.4362936  -8.567  < 2e-16 ***
    ## Account.length          0.0007981  0.0008827   0.904 0.365890    
    ## Area.code415           -0.0952939  0.0883866  -1.078 0.280968    
    ## Area.code510           -0.1299510  0.1012936  -1.283 0.199522    
    ## Number.vmail.messages   0.0401957  0.0108741   3.696 0.000219 ***
    ## Total.day.minutes       0.0123277  0.0006464  19.070  < 2e-16 ***
    ## Total.day.calls         0.0024739  0.0017370   1.424 0.154369    
    ## Total.eve.minutes       0.0054218  0.0007214   7.516 5.64e-14 ***
    ## Total.eve.calls         0.0004061  0.0017745   0.229 0.818989    
    ## Total.night.minutes     0.0028918  0.0007343   3.938 8.21e-05 ***
    ## Total.night.calls       0.0007615  0.0018234   0.418 0.676239    
    ## Total.intl.minutes      0.0787050  0.0131523   5.984 2.18e-09 ***
    ## Total.intl.calls       -0.0854122  0.0146482  -5.831 5.51e-09 ***
    ## Customer.service.calls  0.5822330  0.0262470  22.183  < 2e-16 ***
    ## International.plan_No  -2.4104429  0.1131892 -21.296  < 2e-16 ***
    ## Voice.mail.plan_Yes    -2.0061683  0.3437203  -5.837 5.33e-09 ***
    ## Voice.mail.plan_No             NA         NA      NA       NA    
    ## International.plan_Yes         NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6316  on 2665  degrees of freedom
    ## Residual deviance: 4740  on 2650  degrees of freedom
    ## AIC: 4823.7
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
df_test_log_w <- df_test
df_test_log_w$predicted_prob <- predict(log_model_w, newdata = df_test_log_w, type = "response")
df_test_log_w$predicted_class <- ifelse(df_test_log_w$predicted_prob > 0.45, 1, 0)

conf_log_w <- confusionMatrix(factor(df_test_log_w$predicted_class), factor(df_test_log_w$Churn))
conf_log_w
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 417  18
    ##          1 155  77
    ##                                           
    ##                Accuracy : 0.7406          
    ##                  95% CI : (0.7056, 0.7735)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0.3369          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.7290          
    ##             Specificity : 0.8105          
    ##          Pos Pred Value : 0.9586          
    ##          Neg Pred Value : 0.3319          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.6252          
    ##    Detection Prevalence : 0.6522          
    ##       Balanced Accuracy : 0.7698          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_log_w$byClass["Pos Pred Value"], conf_log_w$byClass["Sensitivity"])
```

    ## [1] 0.8282026

``` r
quick_roc_auc(log_model_w, df_test_log_w, df_test_log_w$Churn)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

    ## Area under the curve: 0.831

**Performance Metrics:**

- **Accuracy:** 76.98%

- **Precision:** 95.86%

- **Recall:** 72.90%

- **F1-score:** 82.82026%

- **AUC:** 83.1%

### Ridge Regression

``` r
x_ridge <- model.matrix(Churn ~ . , df_train)[, -1]
y_ridge <- df_train$Churn

cv_ridge <- cv.glmnet(x_ridge, y_ridge, alpha = 0, family = "binomial")

best_lambda_r <- cv_ridge$lambda.min
print(best_lambda_r)
```

    ## [1] 0.009785387

``` r
ridge_model <- glmnet(x_ridge, y_ridge, alpha = 0, lambda = best_lambda_r, family = "binomial")
summary(ridge_model)
```

    ##            Length Class     Mode     
    ## a0          1     -none-    numeric  
    ## beta       17     dgCMatrix S4       
    ## df          1     -none-    numeric  
    ## dim         2     -none-    numeric  
    ## lambda      1     -none-    numeric  
    ## dev.ratio   1     -none-    numeric  
    ## nulldev     1     -none-    numeric  
    ## npasses     1     -none-    numeric  
    ## jerr        1     -none-    numeric  
    ## offset      1     -none-    logical  
    ## classnames  2     -none-    character
    ## call        6     -none-    call     
    ## nobs        1     -none-    numeric

``` r
x_test_r <- model.matrix(Churn ~ . -1, data = df_test)[, -1]
y_test_r <- df_test$Churn

ridge_pred <- predict(ridge_model, newx = x_test_r, type = "response")
ridge_pred_class <- ifelse(ridge_pred > 0.45, 1, 0)
ridge_pred_class <- factor(ridge_pred_class, levels = levels(factor(y_test_r)))

conf_matrix_ridge <- confusionMatrix(ridge_pred_class, factor(y_test_r))
print(conf_matrix_ridge)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 562  87
    ##          1  10   8
    ##                                           
    ##                Accuracy : 0.8546          
    ##                  95% CI : (0.8255, 0.8805)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : 0.6138          
    ##                                           
    ##                   Kappa : 0.1008          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.194e-14       
    ##                                           
    ##             Sensitivity : 0.98252         
    ##             Specificity : 0.08421         
    ##          Pos Pred Value : 0.86595         
    ##          Neg Pred Value : 0.44444         
    ##              Prevalence : 0.85757         
    ##          Detection Rate : 0.84258         
    ##    Detection Prevalence : 0.97301         
    ##       Balanced Accuracy : 0.53336         
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_matrix_ridge$byClass["Pos Pred Value"], conf_matrix_ridge$byClass["Sensitivity"])
```

    ## [1] 0.9205569

``` r
quick_roc_auc_ridge <- function(model, x_test, y_test, positive_class = 1, is_glmnet = FALSE) {
  if (is_glmnet) {
    probs <- predict(model, newx = x_test, type = "response")[, 1]
  } else {
    probs <- predict(model, newdata = x_test, type = "response")
  }
  
  y_test <- factor(y_test, levels = c(1 - positive_class, positive_class))
  roc_obj <- pROC::roc(y_test, probs)
  plot(roc_obj, col = "blue", lwd = 2, main = paste("AUC =", round(auc(roc_obj), 3)))
  return(auc(roc_obj))
}
quick_roc_auc_ridge(ridge_model, x_test_r, y_test_r, is_glmnet = TRUE)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

    ## Area under the curve: 0.7638

**Performance Metrics:**

- **Accuracy:** 85.46%

- **Precision:** 86.595%

- **Recall:** 98.252%

- **F1-score:** 92.05569%

- **AUC:** 76.4%

### Lasso Regression

``` r
x_lasso <- model.matrix(Churn ~ . , df_train)[, -1]
y_lasso <- df_train$Churn

cv_lasso <- cv.glmnet(x_lasso, y_lasso, alpha = 1, family = "binomial")

best_lambda_l <- cv_ridge$lambda.min
print(best_lambda_l)
```

    ## [1] 0.009785387

``` r
lasso_model <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = best_lambda_l, family = "binomial")
summary(lasso_model)
```

    ##            Length Class     Mode     
    ## a0          1     -none-    numeric  
    ## beta       17     dgCMatrix S4       
    ## df          1     -none-    numeric  
    ## dim         2     -none-    numeric  
    ## lambda      1     -none-    numeric  
    ## dev.ratio   1     -none-    numeric  
    ## nulldev     1     -none-    numeric  
    ## npasses     1     -none-    numeric  
    ## jerr        1     -none-    numeric  
    ## offset      1     -none-    logical  
    ## classnames  2     -none-    character
    ## call        6     -none-    call     
    ## nobs        1     -none-    numeric

``` r
x_test_l <- model.matrix(Churn ~ . -1, data = df_test)[, -1]
y_test_l <- df_test$Churn

lasso_pred <- predict(lasso_model, newx = x_test_l, type = "response")
lasso_pred_class <- ifelse(lasso_pred > 0.45, 1, 0)
lasso_pred_class <- factor(lasso_pred_class, levels = levels(factor(y_test_l)))

conf_matrix_lasso <- confusionMatrix(lasso_pred_class, factor(y_test_l))
print(conf_matrix_lasso)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 559  83
    ##          1  13  12
    ##                                           
    ##                Accuracy : 0.8561          
    ##                  95% CI : (0.8271, 0.8818)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : 0.571           
    ##                                           
    ##                   Kappa : 0.1495          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.891e-12       
    ##                                           
    ##             Sensitivity : 0.9773          
    ##             Specificity : 0.1263          
    ##          Pos Pred Value : 0.8707          
    ##          Neg Pred Value : 0.4800          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.8381          
    ##    Detection Prevalence : 0.9625          
    ##       Balanced Accuracy : 0.5518          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_matrix_lasso$byClass["Pos Pred Value"], conf_matrix_lasso$byClass["Sensitivity"])
```

    ## [1] 0.9209226

``` r
quick_roc_auc_ridge(lasso_model, x_test_l, y_test_l, is_glmnet = TRUE)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

    ## Area under the curve: 0.7819

**Performance Metrics:**

- **Accuracy:** 85.61%

- **Precision:** 87.07%

- **Recall:** 97.73%

- **F1-score:** 92.09226%

- **AUC:** 78.2%

## Tree-based Models

### Decision Tree

``` r
df_train_tree <- df_train
df_test_tree <- df_test
df_train_tree$Churn <- as.factor(df_train_tree$Churn)
df_test_tree$Churn <- as.factor(df_test_tree$Churn)

tree_model <- rpart(Churn ~ ., 
                    data = df_train_tree, 
                    method = "class",
                    control = rpart.control(minsplit = 10, cp = 0.01))
rpart.plot(tree_model, box.palette = "auto", nn = TRUE)
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
plot_importance <- function(model, top_n = NULL) {
  if (inherits(model, "rpart")) {
    imp <- model$variable.importance
    importance_df <- data.frame(
      Feature = names(imp),
      Importance = as.numeric(imp)
    )
  } else if (inherits(model, "randomForest")) {
    imp <- importance(model)
    importance_df <- data.frame(
      Feature = rownames(imp),
      Importance = imp[, 1]
    )
  } else if (inherits(model, "ranger")) {
    imp <- model$variable.importance
    importance_df <- data.frame(
      Feature = names(imp),
      Importance = as.numeric(imp)
    )
  } else if (inherits(model, "xgb.Booster")) {
    imp <- xgboost::xgb.importance(model = model)
    colnames(imp)[colnames(imp) == "Gain"] <- "Importance"
    importance_df <- imp[, c("Feature", "Importance")]
  
  } else {
    stop("Model type not supported: only rpart, randomForest, ranger, and xgboost are supported.")
  }
  importance_df <- importance_df[order(-importance_df$Importance), ]
  if (!is.null(top_n)) {
    importance_df <- head(importance_df, top_n)
  }
  ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    labs(
      title = "Feature Importances",
      x = "Feature",
      y = "Importance"
    ) +
    theme_minimal()
}
```

``` r
plot_importance(tree_model)
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
tree_pred <- predict(tree_model, df_test_tree, type = "class")
conf_tree <- confusionMatrix(tree_pred, df_test_tree$Churn)
conf_tree
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 563  20
    ##          1   9  75
    ##                                           
    ##                Accuracy : 0.9565          
    ##                  95% CI : (0.9382, 0.9707)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.813           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.06332         
    ##                                           
    ##             Sensitivity : 0.9843          
    ##             Specificity : 0.7895          
    ##          Pos Pred Value : 0.9657          
    ##          Neg Pred Value : 0.8929          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.8441          
    ##    Detection Prevalence : 0.8741          
    ##       Balanced Accuracy : 0.8869          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_tree$byClass["Pos Pred Value"], conf_tree$byClass["Sensitivity"])
```

    ## [1] 0.9748918

``` r
quick_roc_auc_prob <- function(model, x_test, y_test, positive_class = 1) {
  probs <- predict(model, newdata = x_test, type = "prob")[, as.character(positive_class)]
  y_test <- factor(y_test, levels = c(1 - positive_class, positive_class))
  roc_obj <- roc(y_test, probs)
  plot(roc_obj, col = "blue", lwd = 2, main = paste("AUC =", round(auc(roc_obj), 3)))
  return(auc(roc_obj))
}
```

``` r
quick_roc_auc_prob(tree_model, df_test_tree, df_test_tree$Churn)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

    ## Area under the curve: 0.8938

**Performance Metrics:**

- **Accuracy:** 95.65%

- **Precision:** 96.57%

- **Recall:** 98.43%

- **F1-score:** 97.48918%

- **AUC:** 89.4%

### Random Forest

``` r
df_train_rf <- df_train
df_test_rf <- df_test
df_train_rf$Churn <- as.factor(df_train_rf$Churn)
df_test_rf$Churn <- as.factor(df_test_rf$Churn)

rf_model <- randomForest(Churn ~ ., data = df_train_rf, ntree = 100, importance = TRUE)
```

``` r
plot_rf_importance <- function(model, top_n = NULL) {
  if (inherits(model, "randomForest")) {
    imp_matrix <- randomForest::importance(model)
    importance_df <- data.frame(
      Feature = rownames(imp_matrix),
      Importance = imp_matrix[, "MeanDecreaseGini"]
    )
  } else if (inherits(model, "rpart")) {
    imp <- model$variable.importance
    importance_df <- data.frame(
      Feature = names(imp),
      Importance = as.numeric(imp)
    )
  } else if (inherits(model, "xgb.Booster")) {
    imp <- xgboost::xgb.importance(model = model)
    colnames(imp)[colnames(imp) == "Gain"] <- "Importance"
    importance_df <- imp[, c("Feature", "Importance")]
    
  } else {
    stop("Unsupported model type.")
  }
  importance_df <- importance_df[order(-importance_df$Importance), ]
  if (!is.null(top_n)) {
    importance_df <- head(importance_df, top_n)
  }
  ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    labs(
      title = "Feature Importances",
      x = "Feature",
      y = "Importance"
    ) +
    theme_minimal()
}

plot_rf_importance(rf_model)
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
rf_pred <- predict(rf_model, newdata = df_test_rf, type = "response")

conf_rf <- confusionMatrix(rf_pred, df_test_rf$Churn)
conf_rf
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 569  31
    ##          1   3  64
    ##                                           
    ##                Accuracy : 0.949           
    ##                  95% CI : (0.9295, 0.9644)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : 2.125e-14       
    ##                                           
    ##                   Kappa : 0.7621          
    ##                                           
    ##  Mcnemar's Test P-Value : 3.649e-06       
    ##                                           
    ##             Sensitivity : 0.9948          
    ##             Specificity : 0.6737          
    ##          Pos Pred Value : 0.9483          
    ##          Neg Pred Value : 0.9552          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.8531          
    ##    Detection Prevalence : 0.8996          
    ##       Balanced Accuracy : 0.8342          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_rf$byClass["Pos Pred Value"], conf_rf$byClass["Sensitivity"])
```

    ## [1] 0.9709898

``` r
quick_roc_auc_prob(rf_model, df_test_rf, df_test_rf$Churn)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

    ## Area under the curve: 0.9309

**Performance Metrics:**

- **Accuracy:** 94.9%

- **Precision:** 94.83%

- **Recall:** 99.48%

- **F1-score:** 97.09898%

- **AUC:** 93.1%

### Gradient Booster

``` r
df_train_gb <- df_train
df_test_gb <- df_test
df_train_gb$Churn <- as.factor(df_train_gb$Churn)
df_test_gb$Churn <- as.factor(df_test_gb$Churn)

df_train_matrix <- model.matrix(Churn ~ . - 1, data = df_train_gb)
df_test_matrix <- model.matrix(Churn ~ . - 1, data = df_test_gb)

train_features <- colnames(df_train_matrix)

missing_cols <- setdiff(train_features, colnames(df_test_matrix))
for (col in missing_cols) {
  df_test_matrix <- cbind(df_test_matrix, setNames(rep(0, nrow(df_test_matrix)), col))
}

extra_cols <- setdiff(colnames(df_test_matrix), train_features)
df_test_matrix <- df_test_matrix[, !(colnames(df_test_matrix) %in% extra_cols)]
df_test_matrix <- df_test_matrix[, train_features]

train_label <- as.numeric(df_train_gb$Churn) - 1
test_label <- as.numeric(df_test_gb$Churn) - 1

dtrain <- xgb.DMatrix(data = df_train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = df_test_matrix, label = test_label)

params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

watchlist <- list(train = dtrain, eval = dtest)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = watchlist,
  early_stopping_rounds = 10,
  print_every_n = 10
)
```

    ## [1]  train-auc:0.906012  eval-auc:0.913839 
    ## Multiple eval metrics are present. Will use eval_auc for early stopping.
    ## Will train until eval_auc hasn't improved in 10 rounds.
    ## 
    ## [11] train-auc:0.952283  eval-auc:0.931008 
    ## [21] train-auc:0.971776  eval-auc:0.931487 
    ## Stopping. Best iteration:
    ## [20] train-auc:0.966533  eval-auc:0.935057

``` r
plot_importance(xgb_model)
```

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

``` r
gb_probs <- predict(xgb_model, newdata = df_test_matrix)
gb_pred <- ifelse(gb_probs > 0.5, 1, 0)

conf_gb <- confusionMatrix(factor(gb_pred), factor(test_label))
conf_gb
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 565  23
    ##          1   7  72
    ##                                           
    ##                Accuracy : 0.955           
    ##                  95% CI : (0.9364, 0.9695)
    ##     No Information Rate : 0.8576          
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.802           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.00617         
    ##                                           
    ##             Sensitivity : 0.9878          
    ##             Specificity : 0.7579          
    ##          Pos Pred Value : 0.9609          
    ##          Neg Pred Value : 0.9114          
    ##              Prevalence : 0.8576          
    ##          Detection Rate : 0.8471          
    ##    Detection Prevalence : 0.8816          
    ##       Balanced Accuracy : 0.8728          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
f1_score(conf_gb$byClass["Pos Pred Value"], conf_gb$byClass["Sensitivity"])
```

    ## [1] 0.9741379

``` r
quick_roc_auc(xgb_model, df_test_matrix, df_test_gb$Churn)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

![](SEC-1-SA2-GROUP-4-BAYBAYON,-D--MAYOL,-J_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

    ## Area under the curve: 0.9351

**Performance Metrics:**

- **Accuracy:** 95.5%

- **Precision:** 96.09%

- **Recall:** 98.78%

- **F1-score:** 97.41379%

- **AUC:** 93.5%

# 4. **Conclusion and Interpretation**

Among the models evaluated for predicting customer churn, gradient
boosting and decision tree methods delivered the strongest overall
performance. Both achieved high accuracy—around 95.6%—with AUC values
near 0.93 and F1-scores close to 0.98, demonstrating excellent ability
to correctly identify customers likely to churn while minimizing false
positives. Random forest also showed strong results with a good balance
of accuracy and AUC. These tree-based models effectively capture
nonlinear relationships and interactions from an extensive amount of
variables which is extremely crucial in churn prediction. In contrast,
linear models such as logistic regression, ridge, and lasso regression,
while offering greater interpretability, showed lower accuracy (74% to
86%) and moderate AUC scores, reflecting their limited capacity to model
complex patterns and class imbalance in churn data.

The bias-variance trade-off is evident in these results: linear models
tend to have higher bias and lower variance, making them more stable but
prone to underfitting. Ensemble tree methods like gradient boosting and
random forest reduce bias while managing variance through aggregation,
resulting in superior predictive performance, though at some cost to
interpretability. However, feature importance analysis across these
models consistently highlights total call minutes during the day and
evening, the number of customer service calls, and total international
call minutes as the most influential predictors.

For a telecom company aiming to reduce churn, gradient boosting or
random forest models are recommended due to their strong predictive
power and ability to identify at-risk customers accurately. These models
enable focused retention efforts by targeting customers with high usage
patterns or frequent service calls, maximizing marketing and operational
effectiveness. At the same time, the consistent importance of specific
usage metrics as mentioned prior offers interpretability that supports
strategic decision-making. For instance, the number of customer service
calls clearly shows that consistent complaints in service can lead to
higher churn rates. If interpretability remains a priority, decision
trees or linear models can help communicate churn drivers clearly to
stakeholders. Overall, **gradient boosting** stands out as the best
choice to balance predictive accuracy with actionable insights.
