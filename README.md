-   [Code File Basics](#code-file-basics)
    -   [Code Header](#code-header)
    -   [Clear Environment and Load packages](#clear-environment-and-load-packages)
-   [Load Data to begin EDA](#load-data-to-begin-eda)
    -   [Analyse the Structure & Summary of data](#analyse-the-structure-summary-of-data)
    -   [Analyse the Visual Summary of data](#analyse-the-visual-summary-of-data)
    -   [Plot the Categorical variables](#plot-the-categorical-variables)
-   [Detailed EDA to examine the observations :](#detailed-eda-to-examine-the-observations)
    -   [How different variables are Correlated?](#how-different-variables-are-correlated)
    -   [Employed/Unemployed Labour Force by Education Attainment](#employedunemployed-labour-force-by-education-attainment)
    -   [Labour Force Participation by Race/Ethnicity](#labour-force-participation-by-raceethnicity)
    -   [Impact of Higher education on earnings](#impact-of-higher-education-on-earnings)
    -   [Earnings by Educational Attainment for Employed Individuals](#earnings-by-educational-attainment-for-employed-individuals)
-   [Subset the Data that satisfies the sample criteria](#subset-the-data-that-satisfies-the-sample-criteria)
    -   [How do Earnings vary by Age? Do older people earn more?](#how-do-earnings-vary-by-age-do-older-people-earn-more)
    -   [How do Earnings vary by Race?](#how-do-earnings-vary-by-race)
    -   [How do Earnings vary by Gender? Do Male(s) earn more than Female(s)?](#how-do-earnings-vary-by-gender-do-males-earn-more-than-females)
    -   [How do Earnings vary by Educational Attainment? Do people with higher degrees earn more?](#how-do-earnings-vary-by-educational-attainment-do-people-with-higher-degrees-earn-more)
    -   [How do Earnings vary by Marital Status? Do married people earn more?](#how-do-earnings-vary-by-marital-status-do-married-people-earn-more)
    -   [How does earnings vary by marital Status and Gender?](#how-does-earnings-vary-by-marital-status-and-gender)
    -   [How do Earnings vary by Number of hours worked? Do people who work more tends to earn more?](#how-do-earnings-vary-by-number-of-hours-worked-do-people-who-work-more-tends-to-earn-more)
    -   [How does Eductaional Attainment vary by Race?](#how-does-eductaional-attainment-vary-by-race)
    -   [How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Race?](#how-do-earnings-vary-by-eductaional-attainment-does-the-premium-for-higher-educational-attainment-vary-by-race)
    -   [How does Eductaional Attainment vary by Gender?](#how-does-eductaional-attainment-vary-by-gender)
    -   [How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Gender?](#how-do-earnings-vary-by-eductaional-attainment-does-the-premium-for-higher-educational-attainment-vary-by-gender)
    -   [How does Race vary by Gender?](#how-does-race-vary-by-gender)
    -   [How do Earnings vary by Race? Does the premium for Race vary by Gender?](#how-do-earnings-vary-by-race-does-the-premium-for-race-vary-by-gender)
    -   [Descriptive Statistics(Mean/Median, Sd) of Earning, Usual Weekly Hours & Age by Education Attainment & Race](#descriptive-statisticsmeanmedian-sd-of-earning-usual-weekly-hours-age-by-education-attainment-race)
    -   [Preliminary Model - Remove Weekly hours](#preliminary-model---remove-weekly-hours)
    -   [Model Estimation](#model-estimation)
    -   [Perform BPG Test](#perform-bpg-test)
    -   [Correct Standard Errors by generating robust standard errors](#correct-standard-errors-by-generating-robust-standard-errors)
    -   [Interpretation of age](#interpretation-of-age)

------------------------------------------------------------------------

Code File Basics
----------------

### Code Header

``` r
# Course: ECON 5300
# Title: Labour Market Analysis 
# Purpose: 
# Research Question: 
```

### Clear Environment and Load packages

``` r
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear working directory (remove all objects)
rm(list=ls(all=TRUE)) 

# Load packages
library(tidyverse)
library(gridExtra)
library(GGally)
library(knitr)
library(grid)
library(reshape)
library(reshape2)
library(psych)
library(corrplot)
library(lmtest)
library(sandwich)
```

Load Data to begin EDA
----------------------

``` r
# Import LMA data
LMA.Data <- read.csv("ACS Data for LMA Project-2.csv", header = TRUE)
```

### Analyse the Structure & Summary of data

``` r
# Structure of the dataset
str(LMA.Data)
```

    ## 'data.frame':    64999 obs. of  26 variables:
    ##  $ Age                                   : int  18 53 61 32 49 58 18 37 49 58 ...
    ##  $ Earnings.Past.12.Months               : int  1200 0 0 350 0 0 0 0 0 43500 ...
    ##  $ Usual.Weekly.Hours                    : int  16 NA 70 24 30 NA NA NA NA 37 ...
    ##  $ Female                                : int  0 1 0 1 0 0 0 1 0 0 ...
    ##  $ Married                               : int  0 0 1 0 0 1 0 0 1 1 ...
    ##  $ No.High.School.Degree                 : int  1 0 0 0 0 1 1 0 0 0 ...
    ##  $ High.School.Degree.or.GED             : int  0 1 0 0 0 0 0 0 1 0 ...
    ##  $ Some.College                          : int  0 0 1 1 0 0 0 0 0 0 ...
    ##  $ Associates.Degree                     : int  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ Bachelors.Degree                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Masters.Degree                        : int  0 0 0 0 1 0 0 1 0 0 ...
    ##  $ Professional.Degree                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Doctorate                             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Educational.Attainment                : Factor w/ 8 levels "Associates Degree",..: 6 4 8 8 5 6 6 5 4 1 ...
    ##  $ Employed                              : int  1 0 1 1 1 0 0 0 1 1 ...
    ##  $ White                                 : int  1 0 1 1 1 1 1 1 1 1 ...
    ##  $ Black                                 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ American.Indian.or.Native.American    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Asian                                 : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Hawaiian.or.Pacific.Islander          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Other.Race                            : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Biracial                              : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Hispanic                              : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ Race.Ethnicity                        : Factor w/ 8 levels "Asian","Biracial",..: 8 5 8 8 8 8 8 8 8 8 ...
    ##  $ Worked.40..Weeks.During.Past.12.Months: int  0 0 1 0 1 0 0 0 0 1 ...
    ##  $ Worked.35..Hours.in.a.Typical.Week    : int  0 0 1 0 0 0 0 0 0 1 ...

``` r
# Summary of the data set ( i.e. Descriptive Statistics of the data)
summary(LMA.Data)
```

    ##       Age        Earnings.Past.12.Months Usual.Weekly.Hours
    ##  Min.   :18.00   Min.   :     0          Min.   : 1.00     
    ##  1st Qu.:30.00   1st Qu.:     0          1st Qu.:35.00     
    ##  Median :43.00   Median : 16000          Median :40.00     
    ##  Mean   :41.69   Mean   : 29776          Mean   :38.51     
    ##  3rd Qu.:54.00   3rd Qu.: 42000          3rd Qu.:42.00     
    ##  Max.   :64.00   Max.   :577000          Max.   :99.00     
    ##                                          NA's   :16131     
    ##      Female          Married       No.High.School.Degree
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000       
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000       
    ##  Median :1.0000   Median :1.0000   Median :0.0000       
    ##  Mean   :0.5082   Mean   :0.5313   Mean   :0.1234       
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000       
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000       
    ##                                                         
    ##  High.School.Degree.or.GED  Some.College    Associates.Degree
    ##  Min.   :0.000             Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.000             1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Median :0.000             Median :0.0000   Median :0.00000  
    ##  Mean   :0.276             Mean   :0.2485   Mean   :0.08063  
    ##  3rd Qu.:1.000             3rd Qu.:0.0000   3rd Qu.:0.00000  
    ##  Max.   :1.000             Max.   :1.0000   Max.   :1.00000  
    ##                                                              
    ##  Bachelors.Degree Masters.Degree    Professional.Degree   Doctorate      
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000     Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000     1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.00000   Median :0.00000     Median :0.00000  
    ##  Mean   :0.1738   Mean   :0.06951   Mean   :0.01788     Mean   :0.01025  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000     3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000     Max.   :1.00000  
    ##                                                                          
    ##            Educational.Attainment    Employed          White       
    ##  High School Degree   :17939      Min.   :0.0000   Min.   :0.0000  
    ##  Some College         :16152      1st Qu.:1.0000   1st Qu.:0.0000  
    ##  Bachelors Degree     :11299      Median :1.0000   Median :1.0000  
    ##  No High School Degree: 8022      Mean   :0.8467   Mean   :0.6749  
    ##  Associates Degree    : 5241      3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Masters Degree       : 4518      Max.   :1.0000   Max.   :1.0000  
    ##  (Other)              : 1828                                       
    ##      Black        American.Indian.or.Native.American     Asian        
    ##  Min.   :0.0000   Min.   :0.000000                   Min.   :0.00000  
    ##  1st Qu.:0.0000   1st Qu.:0.000000                   1st Qu.:0.00000  
    ##  Median :0.0000   Median :0.000000                   Median :0.00000  
    ##  Mean   :0.1107   Mean   :0.009754                   Mean   :0.05057  
    ##  3rd Qu.:0.0000   3rd Qu.:0.000000                   3rd Qu.:0.00000  
    ##  Max.   :1.0000   Max.   :1.000000                   Max.   :1.00000  
    ##                                                                       
    ##  Hawaiian.or.Pacific.Islander   Other.Race          Biracial      
    ##  Min.   :0.000000             Min.   :0.000000   Min.   :0.00000  
    ##  1st Qu.:0.000000             1st Qu.:0.000000   1st Qu.:0.00000  
    ##  Median :0.000000             Median :0.000000   Median :0.00000  
    ##  Mean   :0.001123             Mean   :0.001077   Mean   :0.01623  
    ##  3rd Qu.:0.000000             3rd Qu.:0.000000   3rd Qu.:0.00000  
    ##  Max.   :1.000000             Max.   :1.000000   Max.   :1.00000  
    ##                                                                   
    ##     Hispanic              Race.Ethnicity 
    ##  Min.   :0.0000   White          :43865  
    ##  1st Qu.:0.0000   Hispanic       : 8819  
    ##  Median :0.0000   Black          : 7196  
    ##  Mean   :0.1357   Asian          : 3287  
    ##  3rd Qu.:0.0000   Biracial       : 1055  
    ##  Max.   :1.0000   Native American:  634  
    ##                   (Other)        :  143  
    ##  Worked.40..Weeks.During.Past.12.Months Worked.35..Hours.in.a.Typical.Week
    ##  Min.   :0.0000                         Min.   :0.0000                    
    ##  1st Qu.:0.0000                         1st Qu.:0.0000                    
    ##  Median :1.0000                         Median :1.0000                    
    ##  Mean   :0.6088                         Mean   :0.5721                    
    ##  3rd Qu.:1.0000                         3rd Qu.:1.0000                    
    ##  Max.   :1.0000                         Max.   :1.0000                    
    ## 

``` r
# Check the details of different variables
describe(LMA.Data)
```

    ##                                        vars     n     mean       sd median
    ## Age                                       1 64999    41.69    13.73     43
    ## Earnings.Past.12.Months                   2 64999 29776.37 45915.83  16000
    ## Usual.Weekly.Hours                        3 48868    38.51    12.85     40
    ## Female                                    4 64999     0.51     0.50      1
    ## Married                                   5 64999     0.53     0.50      1
    ## No.High.School.Degree                     6 64999     0.12     0.33      0
    ## High.School.Degree.or.GED                 7 64999     0.28     0.45      0
    ## Some.College                              8 64999     0.25     0.43      0
    ## Associates.Degree                         9 64999     0.08     0.27      0
    ## Bachelors.Degree                         10 64999     0.17     0.38      0
    ## Masters.Degree                           11 64999     0.07     0.25      0
    ## Professional.Degree                      12 64999     0.02     0.13      0
    ## Doctorate                                13 64999     0.01     0.10      0
    ## Educational.Attainment*                  14 64999     4.76     2.36      4
    ## Employed                                 15 64999     0.85     0.36      1
    ## White                                    16 64999     0.67     0.47      1
    ## Black                                    17 64999     0.11     0.31      0
    ## American.Indian.or.Native.American       18 64999     0.01     0.10      0
    ## Asian                                    19 64999     0.05     0.22      0
    ## Hawaiian.or.Pacific.Islander             20 64999     0.00     0.03      0
    ## Other.Race                               21 64999     0.00     0.03      0
    ## Biracial                                 22 64999     0.02     0.13      0
    ## Hispanic                                 23 64999     0.14     0.34      0
    ## Race.Ethnicity*                          24 64999     6.56     2.25      8
    ## Worked.40..Weeks.During.Past.12.Months   25 64999     0.61     0.49      1
    ## Worked.35..Hours.in.a.Typical.Week       26 64999     0.57     0.49      1
    ##                                         trimmed      mad min    max  range
    ## Age                                       41.90    17.79  18     64     46
    ## Earnings.Past.12.Months                21173.62 23721.60   0 577000 577000
    ## Usual.Weekly.Hours                        38.82     7.41   1     99     98
    ## Female                                     0.51     0.00   0      1      1
    ## Married                                    0.54     0.00   0      1      1
    ## No.High.School.Degree                      0.03     0.00   0      1      1
    ## High.School.Degree.or.GED                  0.22     0.00   0      1      1
    ## Some.College                               0.19     0.00   0      1      1
    ## Associates.Degree                          0.00     0.00   0      1      1
    ## Bachelors.Degree                           0.09     0.00   0      1      1
    ## Masters.Degree                             0.00     0.00   0      1      1
    ## Professional.Degree                        0.00     0.00   0      1      1
    ## Doctorate                                  0.00     0.00   0      1      1
    ## Educational.Attainment*                    4.81     2.97   1      8      7
    ## Employed                                   0.93     0.00   0      1      1
    ## White                                      0.72     0.00   0      1      1
    ## Black                                      0.01     0.00   0      1      1
    ## American.Indian.or.Native.American         0.00     0.00   0      1      1
    ## Asian                                      0.00     0.00   0      1      1
    ## Hawaiian.or.Pacific.Islander               0.00     0.00   0      1      1
    ## Other.Race                                 0.00     0.00   0      1      1
    ## Biracial                                   0.00     0.00   0      1      1
    ## Hispanic                                   0.04     0.00   0      1      1
    ## Race.Ethnicity*                            6.98     0.00   1      8      7
    ## Worked.40..Weeks.During.Past.12.Months     0.64     0.00   0      1      1
    ## Worked.35..Hours.in.a.Typical.Week         0.59     0.00   0      1      1
    ##                                         skew kurtosis     se
    ## Age                                    -0.13    -1.21   0.05
    ## Earnings.Past.12.Months                 4.28    28.66 180.10
    ## Usual.Weekly.Hours                     -0.01     2.05   0.06
    ## Female                                 -0.03    -2.00   0.00
    ## Married                                -0.13    -1.98   0.00
    ## No.High.School.Degree                   2.29     3.24   0.00
    ## High.School.Degree.or.GED               1.00    -1.00   0.00
    ## Some.College                            1.16    -0.65   0.00
    ## Associates.Degree                       3.08     7.49   0.00
    ## Bachelors.Degree                        1.72     0.96   0.00
    ## Masters.Degree                          3.39     9.46   0.00
    ## Professional.Degree                     7.28    50.95   0.00
    ## Doctorate                               9.73    92.60   0.00
    ## Educational.Attainment*                 0.05    -1.23   0.01
    ## Employed                               -1.92     1.70   0.00
    ## White                                  -0.75    -1.44   0.00
    ## Black                                   2.48     4.16   0.00
    ## American.Indian.or.Native.American      9.98    97.53   0.00
    ## Asian                                   4.10    14.83   0.00
    ## Hawaiian.or.Pacific.Islander           29.79   885.37   0.00
    ## Other.Race                             30.42   923.53   0.00
    ## Biracial                                7.66    56.63   0.00
    ## Hispanic                                2.13     2.53   0.00
    ## Race.Ethnicity*                        -1.22     0.03   0.01
    ## Worked.40..Weeks.During.Past.12.Months -0.45    -1.80   0.00
    ## Worked.35..Hours.in.a.Typical.Week     -0.29    -1.92   0.00

Observations :

1.  Earnings probably have right skewed distribution.
2.  Maximum number of individuals have High School degree .
3.  Majority of the individuals in our sample data are White.
4.  Age of the individuals in our sample data varies between 18 to 64.
5.  Usual Weekly Hours of the individuals in our sample data varies between 1 to 99.

### Analyse the Visual Summary of data

``` r
# Visualize numerical variables

par(mfrow = c(3, 3))
hist(LMA.Data$Age, main = "Histogram of Age")
hist(LMA.Data$Earnings.Past.12.Months, main = "Histogram of Earnings")
hist(LMA.Data$Usual.Weekly.Hours, main = "Histogram of Usual.Weekly.Hours")
hist(LMA.Data$Female, main = "Histogram of Gender")
hist(LMA.Data$Married, main = "Histogram of Marital-Status")
hist(LMA.Data$Employed, main = "Histogram of Employment-Status")
hist(LMA.Data$Worked.40..Weeks.During.Past.12.Months, main = "Histogram of Work-duration (40 weeks in a year)")
hist(LMA.Data$Worked.35..Hours.in.a.Typical.Week, main = "Histogram of Work-duration (35 hours in a week)")
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-5-1.png)

Observations :

1.  It is confirmed that the earning have right skewed distribution. Hence we need to take the log in statistical model to normalise the data.
2.  On an average people usually work for 38 hours.

### Plot the Categorical variables

``` r
# Plot factor variables
par(mfrow = c(3, 2))
barplot(table(LMA.Data$Educational.Attainment), main = "Educational Attainment")
barplot(table(LMA.Data$Race.Ethnicity), main = "Race/Ethnicity")
barplot(table(LMA.Data$Married), main = "Marital Status")
barplot(table(LMA.Data$Female), main = "Gender")
barplot(table(LMA.Data$Employed), main = "Employed")
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-6-1.png)

Observations :

1.  Most number of individuals (i.e 17939) just have High School Degree.
2.  Sample Data is equally distributed between male and female.
3.  84.6% of the individuals in the sample data are employed.

Detailed EDA to examine the observations :
------------------------------------------

### How different variables are Correlated?

``` r
LMA.Data %>% select(Age, Earnings.Past.12.Months, Female, Married, Employed) %>% 
    ggpairs()
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-7-1.png)

Observations :

1.  High positive correlation between "Age & Married", "Age & Earnings", "Earnings & Married".
2.  Negative correlation between "Gender & Eanrings".

### Employed/Unemployed Labour Force by Education Attainment

``` r
A <- LMA.Data %>% filter(Employed == 1) %>% group_by(Educational.Attainment) %>% 
    summarise(count = n(), Proportion = sum(count)/nrow(LMA.Data), PercentEmployed = Proportion * 
        100) %>% select(Educational.Attainment, count, PercentEmployed) %>% 
    ggplot(aes(x = reorder(Educational.Attainment, count), y = count, fill = "red")) + 
    geom_bar(stat = "identity", position = "dodge") + guides(fill = FALSE) + 
    geom_text(aes(label = count), size = 5, vjust = -0.1, position = position_dodge(0.9)) + 
    theme_classic() + ggtitle("Employed Labour Force by Education Attainment") + 
    xlab("Education Attainment") + ylab("Count of Employed Individuals") + theme(axis.text.x = element_text(angle = 20, 
    hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, face = "bold", 
    hjust = 0.5)) + theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

B <- LMA.Data %>% filter(Employed == 0) %>% group_by(Educational.Attainment) %>% 
    summarise(count = n(), Proportion = sum(count)/nrow(LMA.Data), PercentEmployed = Proportion * 
        100) %>% select(Educational.Attainment, count, PercentEmployed) %>% 
    ggplot(aes(x = reorder(Educational.Attainment, count), y = count, fill = "red")) + 
    geom_bar(stat = "identity", position = "dodge") + guides(fill = FALSE) + 
    geom_text(aes(label = count), size = 5, vjust = -0.1, position = position_dodge(0.9)) + 
    theme_classic() + ggtitle("Unemployed Labour Force by Education Attainment") + 
    xlab("Education Attainment") + ylab("Count of Unemployed Individuals") + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))

grid.arrange(A, B, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-8-1.png)

Observations :

1.  Only 27 Doctrate Degree Holders are UnEmployed.
2.  Small number of individuals holding post Secondary Degree are UnEmployed.

### Labour Force Participation by Race/Ethnicity

``` r
LMA.Data %>% group_by(Race.Ethnicity) %>% summarise(count = n(), Proportion = sum(count)/nrow(LMA.Data), 
    PercentEmployed = Proportion * 100) %>% select(Race.Ethnicity, count, PercentEmployed) %>% 
    ggplot(aes(x = reorder(Race.Ethnicity, count), y = count, fill = "red")) + 
    geom_bar(stat = "identity", position = "dodge") + guides(fill = FALSE) + 
    geom_text(aes(label = count), size = 5, vjust = -0.1, position = position_dodge(0.9)) + 
    theme_classic() + ggtitle("Labour Force Participation by Race/Ethnicity") + 
    xlab("Race/Ethnicity") + ylab("Count of Economically Active Individuals") + 
    # theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) +
theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-9-1.png)

Observations :

1.  Most of the individuals in the sample data are White.

### Impact of Higher education on earnings

``` r
kable(LMA.Data %>% filter(Earnings.Past.12.Months > 0 & Employed == 1) %>% group_by(Educational.Attainment) %>% 
    summarise(Avg.Earning = round(mean(Earnings.Past.12.Months), 0), Median.Earning = median(Earnings.Past.12.Months), 
        Avg.Weekly.Hours = round(mean(Usual.Weekly.Hours), 1), Median.Weekly.Hours = median(Usual.Weekly.Hours)) %>% 
    arrange(desc(Median.Earning)), align = "l", format = "html", caption = "Higher education results in Good Jobs & Higher Earnings")
```

<table>
<caption>
Higher education results in Good Jobs & Higher Earnings
</caption>
<thead>
<tr>
<th style="text-align:left;">
Educational.Attainment
</th>
<th style="text-align:left;">
Avg.Earning
</th>
<th style="text-align:left;">
Median.Earning
</th>
<th style="text-align:left;">
Avg.Weekly.Hours
</th>
<th style="text-align:left;">
Median.Weekly.Hours
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Professional Degree
</td>
<td style="text-align:left;">
131348
</td>
<td style="text-align:left;">
85000
</td>
<td style="text-align:left;">
45.7
</td>
<td style="text-align:left;">
45
</td>
</tr>
<tr>
<td style="text-align:left;">
Doctorate
</td>
<td style="text-align:left;">
93100
</td>
<td style="text-align:left;">
72000
</td>
<td style="text-align:left;">
43.9
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Masters Degree
</td>
<td style="text-align:left;">
73064
</td>
<td style="text-align:left;">
58000
</td>
<td style="text-align:left;">
41.6
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Bachelors Degree
</td>
<td style="text-align:left;">
58032
</td>
<td style="text-align:left;">
45000
</td>
<td style="text-align:left;">
40.3
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Associates Degree
</td>
<td style="text-align:left;">
39579
</td>
<td style="text-align:left;">
34000
</td>
<td style="text-align:left;">
38.8
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
High School Degree
</td>
<td style="text-align:left;">
29160
</td>
<td style="text-align:left;">
24000
</td>
<td style="text-align:left;">
38.1
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
Some College
</td>
<td style="text-align:left;">
30607
</td>
<td style="text-align:left;">
24000
</td>
<td style="text-align:left;">
36.6
</td>
<td style="text-align:left;">
40
</td>
</tr>
<tr>
<td style="text-align:left;">
No High School Degree
</td>
<td style="text-align:left;">
20683
</td>
<td style="text-align:left;">
16000
</td>
<td style="text-align:left;">
36.5
</td>
<td style="text-align:left;">
40
</td>
</tr>
</tbody>
</table>
### Earnings by Educational Attainment for Employed Individuals

``` r
G1 <- LMA.Data %>% filter(Employed == 1 & Earnings.Past.12.Months > 0) %>% # filter(Educational.Attainment == 'Bachelors Degree' & Race.Ethnicity !=
# 'White') %>%
ggplot(aes(x = Educational.Attainment, y = Earnings.Past.12.Months, fill = "#4271AE")) + 
    geom_boxplot(color = "#1F3552") + scale_y_continuous(name = "Employed Indivuduals Earnings", 
    breaks = seq(4, 577000, 1e+05)) + scale_x_discrete(name = "Educational Attainment") + 
    theme_bw() + guides(fill = FALSE) + theme(axis.text.x = element_text(angle = 20, 
    hjust = 1, vjust = 1)) + ggtitle("Individuals with Professional Degree holder Earns the Most", 
    subtitle = "Earnings by Education Level")

G2 <- LMA.Data %>% filter(Employed == 1 & Earnings.Past.12.Months > 0) %>% ggplot(aes(x = Educational.Attainment, 
    y = Age, fill = "#4271AE")) + geom_boxplot(color = "#1F3552") + scale_y_continuous(name = "Employed Indivuduals Age", 
    breaks = seq(10, 70, 5)) + scale_x_discrete(name = "Educational Attainment") + 
    theme_bw() + guides(fill = FALSE) + theme(axis.text.x = element_text(angle = 20, 
    hjust = 1, vjust = 1)) + ggtitle("Most of the Individuals with Professional or Doctorate or Masters Degree\n are older than other degree holders", 
    subtitle = "Education Level by Age")

grid.arrange(G1, G2, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-11-1.png)

Subset the Data that satisfies the sample criteria
--------------------------------------------------

``` r
LMA.Data <- LMA.Data %>% # Filter the number of observations for which employed individuals have <
# 1000 USD in earnings or the indivuduals are unemployed - 44732
filter(Earnings.Past.12.Months > 1000 & Employed == 1) %>% # Filter the number of individuals those who belongs to either Hawaiian or
# Other Race
filter(!(Race.Ethnicity %in% c("Other Race", "Hawaiian", "Biracial", "Native American")))
```

### How do Earnings vary by Age? Do older people earn more?

``` r
# Create Age Groups
lma_age_data <- LMA.Data %>% mutate(Age_Group = cut(Age, 4)) %>% group_by(Age_Group) %>% 
    summarise(Num_People = n(), Median_Earnings = median(Earnings.Past.12.Months), 
        Pop_percentage = (Num_People/nrow(LMA.Data)) * 100)

# Plot Number of people by Age
g1 <- lma_age_data %>% ggplot(aes(x = Age_Group, y = Pop_percentage)) + geom_bar(stat = "identity", 
    fill = "lightblue") + xlab("Age") + ylab("Percentage Population by Age") + 
    ggtitle("How do Population vary by Age?") + theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 
    1), "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))


# Plot Earnings by Age
g2 <- lma_age_data %>% ggplot(aes(x = Age_Group, y = Median_Earnings)) + geom_bar(stat = "identity", 
    fill = "lightblue") + xlab("Age") + ylab("Median Earnings") + ggtitle("How do Earnings vary by Age?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12))

grid.arrange(g1, g2, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-13-1.png)

Observations :

1.  Number of Employed Individials belong to the Age between 41 to 52.5. They also have highest median earnings.

### How do Earnings vary by Race?

``` r
lma_race_data <- LMA.Data %>% group_by(Race.Ethnicity) %>% summarise(Num_People = n(), 
    Median_Earnings = median(Earnings.Past.12.Months), Pop_percentage = (Num_People/nrow(LMA.Data)) * 
        100)

# Plot Earnings by Race
g3 <- lma_race_data %>% ggplot(aes(x = reorder(Race.Ethnicity, Num_People), 
    y = Pop_percentage)) + geom_bar(stat = "identity", fill = "lightblue") + 
    ylab("Population Percentage") + ggtitle("Population Percentage by Race") + 
    theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 1), 
    "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12), 
        axis.title.x = element_blank())

# Plot Earnings by Race
g4 <- lma_race_data %>% ggplot(aes(x = reorder(Race.Ethnicity, Median_Earnings), 
    y = Median_Earnings)) + geom_bar(stat = "identity", fill = "lightblue") + 
    ylab("Median Earnings") + ggtitle("How do Earnings vary by Race?") + theme_classic() + 
    geom_text(aes(label = Median_Earnings), vjust = -0.3, colour = "black", 
        position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 30, 
    hjust = 1, vjust = 1, size = 12), axis.title.x = element_blank())

grid.arrange(g3, g4, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-14-1.png)

Observations :

1.  Asians have the highest median earnings among White, Black & Hispanic Individuals.

### How do Earnings vary by Gender? Do Male(s) earn more than Female(s)?

``` r
lma_gender_data <- LMA.Data %>% group_by(Female) %>% summarise(Num_People = n(), 
    Median_Earnings = median(Earnings.Past.12.Months), Pop_percentage = round(((Num_People/nrow(LMA.Data)) * 
        100), 1))

# Plot Earnings by Gender
g5 <- lma_gender_data %>% ggplot(aes(x = reorder(Female, Num_People), y = Pop_percentage)) + 
    geom_bar(stat = "identity", fill = "lightblue") + ylab("Population Percentage") + 
    ggtitle("Population Percentage by Gender") + theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 
    1), "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank()) + scale_x_discrete(labels = c(`1` = "Female", 
    `0` = "Male"))

# Plot Earnings by Gender
g6 <- lma_gender_data %>% ggplot(aes(x = reorder(Female, Num_People), y = Median_Earnings)) + 
    geom_bar(stat = "identity", fill = "lightblue") + ylab("Median Earnings") + 
    ggtitle("How do Earnings vary by Gender?") + theme_classic() + geom_text(aes(label = Median_Earnings), 
    vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank()) + scale_x_discrete(labels = c(`1` = "Female", 
    `0` = "Male"))

grid.arrange(g5, g6, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-15-1.png)

Observations :

1.  Males have higher median earnings than Female counterparts.

### How do Earnings vary by Educational Attainment? Do people with higher degrees earn more?

``` r
lma_edu_data <- LMA.Data %>% group_by(Educational.Attainment) %>% summarise(Num_People = n(), 
    Median_Earnings = median(Earnings.Past.12.Months), Pop_percentage = round(((Num_People/nrow(LMA.Data)) * 
        100), 1))

# Plot Earnings by Educational Attainment
g7 <- lma_edu_data %>% ggplot(aes(x = reorder(Educational.Attainment, Num_People), 
    y = Pop_percentage)) + geom_bar(stat = "identity", fill = "lightblue") + 
    ylab("Population Percentage") + ggtitle("Population Percentage by Educational Attainment") + 
    theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 1), 
    "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12), 
        axis.title.x = element_blank())

# Plot Earnings by Educational Attainment
g8 <- lma_edu_data %>% ggplot(aes(x = reorder(Educational.Attainment, Median_Earnings), 
    y = Median_Earnings)) + geom_bar(stat = "identity", fill = "lightblue") + 
    ylab("Median Earnings") + ggtitle("How do Earnings vary by Educational Attainment?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + theme(axis.text.x = element_text(angle = 30, 
    hjust = 1, vjust = 1, size = 12), axis.title.x = element_blank())

grid.arrange(g7, g8, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-16-1.png)

Observations :

1.  Individuals who hold Professional Degree have the highest median earnings.

### How do Earnings vary by Marital Status? Do married people earn more?

``` r
lma_married_data <- LMA.Data %>% group_by(Married) %>% summarise(Num_People = n(), 
    Median_Earnings = median(Earnings.Past.12.Months), Pop_percentage = round(((Num_People/nrow(LMA.Data)) * 
        100), 1))

# Plot Earnings by Marital Status
g9 <- lma_married_data %>% ggplot(aes(x = reorder(Married, Num_People), y = Pop_percentage)) + 
    geom_bar(stat = "identity", fill = "lightblue") + ylab("Population Percentage") + 
    ggtitle("Population Percentage by Marital Status") + theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 
    1), "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank()) + scale_x_discrete(labels = c(`1` = "Married", 
    `0` = "Not Married"))

# Plot Earnings by Marital Status
g10 <- lma_married_data %>% ggplot(aes(x = reorder(Married, Median_Earnings), 
    y = Median_Earnings)) + geom_bar(stat = "identity", fill = "lightblue") + 
    ylab("Median Earnings") + ggtitle("How do Earnings vary by Marital Status?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + theme(axis.title.x = element_blank()) + 
    scale_x_discrete(labels = c(`1` = "Married", `0` = "Not Married"))

grid.arrange(g9, g10, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-17-1.png)

Observations :

1.  Married people have higher median earnings than their counterparts.

### How does earnings vary by marital Status and Gender?

``` r
LMA.Data %>% mutate(Status = as.factor(ifelse(Married == 0 & Female == 0, "UnMarried Male", 
    ifelse(Married == 0 & Female == 1, "UnMarried Female", ifelse(Married == 
        1 & Female == 0, "Married Male", ifelse(Married == 1 & Female == 1, 
        "Married Female", "No")))))) %>% group_by(Status, Educational.Attainment) %>% 
    summarise(Mean.Earnings = mean(Earnings.Past.12.Months)) %>% ggplot(aes(x = Educational.Attainment, 
    y = Mean.Earnings, fill = Status)) + geom_bar(stat = "identity", position = "dodge") + 
    # geom_text(aes(label = Count) , size = 4, vjust = -0.2, position =
# position_dodge(0.9)) +
theme_classic() + ggtitle("On Average Married Man Earns the Most") + theme(axis.text.x = element_text(angle = 25, 
    hjust = 1, vjust = 1))
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-18-1.png)

### How do Earnings vary by Number of hours worked? Do people who work more tends to earn more?

``` r
lma_whours_data <- LMA.Data %>% mutate(WHours_Group = cut(Usual.Weekly.Hours, 
    5)) %>% group_by(WHours_Group) %>% summarise(Num_People = n(), Median_Earnings = median(Earnings.Past.12.Months), 
    Pop_percentage = round(((Num_People/nrow(LMA.Data)) * 100), 1))

# Plot Earnings by Number of hours worked
g11 <- lma_whours_data %>% ggplot(aes(x = WHours_Group, y = Pop_percentage)) + 
    geom_bar(stat = "identity", fill = "lightblue") + ylab("Population Percentage") + 
    ggtitle("Population Percentage by Hours Worked") + theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 
    1), "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

# Plot Earnings by Number of hours worked
g12 <- lma_whours_data %>% ggplot(aes(x = WHours_Group, y = Median_Earnings)) + 
    geom_bar(stat = "identity", fill = "lightblue") + ylab("Median Earnings") + 
    ggtitle("How do Earnings vary by Hours Worked?") + theme_classic() + geom_text(aes(label = Median_Earnings), 
    vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))

grid.arrange(g11, g12, nrow = 1)
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-19-1.png)

Observations :

1.  63.9% of the individuals in the sample data works weekly between 20.6 and 40.2 hours.
2.  People working between 59.8 and 79.4 weekly hours have highest Median Earnings than all other weekly work hour groups.

### How does Eductaional Attainment vary by Race?

``` r
lma_edu_race_data <- LMA.Data %>% group_by(Race.Ethnicity, Educational.Attainment) %>% 
    summarise(Num_Race_People = n(), Median_Earnings = median(Earnings.Past.12.Months))

lma_edu_race_data <- lma_edu_race_data %>% inner_join((lma_race_data %>% select(Num_People, 
    Race.Ethnicity)), by = "Race.Ethnicity") %>% mutate(Pop_percentage = round(((Num_Race_People/Num_People) * 
    100), 1))

# Plot Population Percentage by Eductaion Attainment and Race
lma_edu_race_data %>% ggplot(aes(x = Race.Ethnicity, y = Pop_percentage, fill = Educational.Attainment)) + 
    geom_bar(stat = "identity", position = "dodge") + ylab("Population Percentage") + 
    ggtitle("Population Percentage by Eductaion Attainment and Race") + theme_classic() + 
    geom_text(aes(label = paste(round(Pop_percentage, 1), "%", sep = "")), vjust = -0.3, 
        colour = "black", position = position_dodge(width = 0.9), size = 5) + 
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank()) + theme(legend.position = "bottom") + 
    theme(legend.title = element_text(size = 20), legend.text = element_text(size = 16))
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-20-1.png)

Observations :

1.  37.7% of Hispanic individuals in the sample data have just High School Degree.
2.  Only 1.8% of Black individuals in the sample data have either a Doctorate or Professional Degree.
3.  Only 1.5% of White people in the sample data have Doctorate Degree.
4.  33.4% of Asian people in the sample data have Bachelors Degree.

### How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Race?

``` r
# Plot Earnings by Eductaional Attainment and Race
lma_edu_race_data %>% ggplot(aes(x = Educational.Attainment, y = Median_Earnings/1000, 
    fill = Race.Ethnicity)) + geom_bar(stat = "identity", position = "dodge") + 
    ylab("Median Earnings (in 1000 USD)") + ggtitle("How do Earnings vary by Eductaional Attainment? \nDoes the premium for higher educational attainment vary by Race?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings/1000), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + theme(axis.title.x = element_blank())
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-21-1.png)

Observations :

1.  Asians with Doctorate, Bachelors Degree, Masters Degree and Professional Degree have the highest Median Earnings among all other Races.
2.  Blacks with Professional Degree earns more than Hispanics with Professional Degrees. The difference is large in comparison to the other degrees.

### How does Eductaional Attainment vary by Gender?

``` r
lma_edu_gender_data <- LMA.Data %>% group_by(Female, Educational.Attainment) %>% 
    summarise(Num_Gender_People = n(), Median_Earnings = median(Earnings.Past.12.Months))

lma_edu_gender_data <- lma_edu_gender_data %>% inner_join((lma_edu_data %>% 
    select(Num_People, Educational.Attainment)), by = "Educational.Attainment") %>% 
    mutate(Pop_percentage = round(((Num_Gender_People/Num_People) * 100), 1))

# Plot Population Percentage by Eductaion Attainment and Race
lma_edu_gender_data %>% ggplot(aes(x = Educational.Attainment, y = Pop_percentage, 
    fill = as.factor(Female))) + geom_bar(stat = "identity", position = "dodge") + 
    ylab("Population Percentage") + ggtitle("Population Percentage by Eductaion Attainment and Gender") + 
    theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 1), 
    "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    scale_fill_discrete(name = "Gender", breaks = c("0", "1"), labels = c("Male", 
        "Female")) + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank())
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-22-1.png)

Observations :

1.  More Males hold - Doctorate, High School Degree and Professional Degree than their Females counterparts.

### How do Earnings vary by Eductaional Attainment? Does the premium for higher educational attainment vary by Gender?

``` r
# Plot Earnings by Eductaional Attainment and Gender
lma_edu_gender_data %>% ggplot(aes(x = Educational.Attainment, y = Median_Earnings/1000, 
    fill = as.factor(Female))) + geom_bar(stat = "identity", position = "dodge") + 
    ylab("Median Earnings (in 1000 USD") + ggtitle("How do Earnings vary by Eductaional Attainment? \nDoes the premium for higher educational attainment vary by Gender?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings/1000), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + scale_fill_discrete(name = "Gender", 
    breaks = c("0", "1"), labels = c("Male", "Female")) + theme(legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12)) + theme(axis.title.x = element_blank())
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-23-1.png)

Observations :

1.  Males have higher median earnings than their Female couterparts irrespective of their eductaional degree.

### How does Race vary by Gender?

``` r
lma_race_gender_data <- LMA.Data %>% group_by(Race.Ethnicity, Female) %>% summarise(Num_Race_People = n(), 
    Median_Earnings = median(Earnings.Past.12.Months))

lma_race_gender_data <- lma_race_gender_data %>% inner_join((lma_race_data %>% 
    select(Num_People, Race.Ethnicity)), by = "Race.Ethnicity") %>% mutate(Pop_percentage = round(((Num_Race_People/Num_People) * 
    100), 1))

# Plot Population Percentage by Race and Gender
lma_race_gender_data %>% ggplot(aes(x = Race.Ethnicity, y = Pop_percentage, 
    fill = as.factor(Female))) + geom_bar(stat = "identity", position = "dodge") + 
    ylab("Population Percentage") + ggtitle("Population Percentage by Race and Gender") + 
    theme_classic() + geom_text(aes(label = paste(round(Pop_percentage, 1), 
    "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    scale_fill_discrete(name = "Gender", breaks = c("0", "1"), labels = c("Male", 
        "Female")) + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank())
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-24-1.png)

### How do Earnings vary by Race? Does the premium for Race vary by Gender?

``` r
# Plot Earnings by Race and Gender
lma_race_gender_data %>% ggplot(aes(x = Race.Ethnicity, y = Median_Earnings/1000, 
    fill = as.factor(Female))) + geom_bar(stat = "identity", position = "dodge") + 
    ylab("Median Earnings (in 1000 USD)") + ggtitle("How do Earnings vary by Race? \nDoes the premium for Race vary by Gender?") + 
    theme_classic() + geom_text(aes(label = Median_Earnings/1000), vjust = -0.3, 
    colour = "black", position = position_dodge(width = 0.9), size = 5) + theme(plot.title = element_text(size = 15, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12)) + scale_fill_discrete(name = "Gender", 
    breaks = c("0", "1"), labels = c("Male", "Female")) + theme(legend.title = element_text(size = 15), 
    legend.text = element_text(size = 12)) + theme(axis.title.x = element_blank())
```

![alt text]( https://github.com/SUMansi/EarningGap_Estimation/blob/master/EDA_Figure/unnamed-chunk-25-1.png)

Observations :

1.  Males have higher median earnings than their Female couterparts irrespective of their Race.

### Descriptive Statistics(Mean/Median, Sd) of Earning, Usual Weekly Hours & Age by Education Attainment & Race

``` r
# #Count of Individuals by Race & Education Attainment kable( LMA.Data %>%
# addmargins(xtabs(~ Educational.Attainment + Race.Ethnicity, data =
# LMA.Data)), align = 'l', format = 'html', caption = 'Count of Individuals
# by Race & Education Attainment') #Percentage of Individuals by Race &
# Education Attainment kable( LMA.Data %>% group_by(Educational.Attainment,
# Race.Ethnicity) %>% summarise(Count = n()) %>%
# arrange(Race.Ethnicity,Educational.Attainment) %>%
# group_by(Race.Ethnicity)%>% mutate( LabourForce_Percentage =
# round((Count/sum(Count))* 100,1) ) %>% select(-Count) %>%
# spread(Race.Ethnicity,LabourForce_Percentage) , align = 'l', format =
# 'html', caption = 'Percentage of Individuals by Race & Education
# Attainment')

# Descriptive Statistics

tapply(LMA.Data$Earnings.Past.12.Months, list(LMA.Data$Educational.Attainment, 
    LMA.Data$Race.Ethnicity), median)
```

    ##                        Asian Biracial Black Hawaiian Hispanic
    ## Associates Degree      32000       NA 30000       NA    34600
    ## Bachelors Degree       50000       NA 40000       NA    38000
    ## Doctorate              90000       NA 68000       NA    67000
    ## High School Degree     22500       NA 21000       NA    21600
    ## Masters Degree         73000       NA 55000       NA    54000
    ## No High School Degree  18000       NA 15000       NA    17650
    ## Professional Degree   102500       NA 80000       NA    58000
    ## Some College           23000       NA 22000       NA    21050
    ##                       Native American Other Race White
    ## Associates Degree                  NA         NA 35000
    ## Bachelors Degree                   NA         NA 47000
    ## Doctorate                          NA         NA 70000
    ## High School Degree                 NA         NA 26400
    ## Masters Degree                     NA         NA 59000
    ## No High School Degree              NA         NA 18000
    ## Professional Degree                NA         NA 90000
    ## Some College                       NA         NA 27000

``` r
tapply(LMA.Data$Earnings.Past.12.Months, list(LMA.Data$Educational.Attainment, 
    LMA.Data$Race.Ethnicity), sd)
```

    ##                           Asian Biracial    Black Hawaiian  Hispanic
    ## Associates Degree      32041.49       NA 23740.40       NA  30559.93
    ## Bachelors Degree       56459.34       NA 39051.37       NA  42198.34
    ## Doctorate              89234.55       NA 34194.91       NA  70992.73
    ## High School Degree     22375.98       NA 23451.39       NA  20177.88
    ## Masters Degree         67089.00       NA 48194.72       NA  50365.75
    ## No High School Degree  21219.48       NA 25369.61       NA  16243.08
    ## Professional Degree   125987.77       NA 74378.79       NA 103417.26
    ## Some College           29138.74       NA 23193.38       NA  26046.70
    ##                       Native American Other Race     White
    ## Associates Degree                  NA         NA  34200.05
    ## Bachelors Degree                   NA         NA  60321.35
    ## Doctorate                          NA         NA  81150.64
    ## High School Degree                 NA         NA  29240.69
    ## Masters Degree                     NA         NA  72416.93
    ## No High School Degree              NA         NA  21620.35
    ## Professional Degree                NA         NA 120399.87
    ## Some College                       NA         NA  34142.53

``` r
tapply(LMA.Data$Usual.Weekly.Hours, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), 
    mean)
```

    ##                          Asian Biracial    Black Hawaiian Hispanic
    ## Associates Degree     40.15528       NA 38.59563       NA 38.18182
    ## Bachelors Degree      39.62389       NA 39.85714       NA 39.67816
    ## Doctorate             44.52747       NA 42.34286       NA 42.61111
    ## High School Degree    38.46795       NA 37.40162       NA 38.19471
    ## Masters Degree        40.48649       NA 41.40070       NA 41.69369
    ## No High School Degree 37.93056       NA 35.31852       NA 38.36902
    ## Professional Degree   45.06863       NA 43.33333       NA 42.33784
    ## Some College          35.41477       NA 36.53794       NA 36.93741
    ##                       Native American Other Race    White
    ## Associates Degree                  NA         NA 39.15270
    ## Bachelors Degree                   NA         NA 40.93033
    ## Doctorate                          NA         NA 44.10624
    ## High School Degree                 NA         NA 39.04686
    ## Masters Degree                     NA         NA 42.13823
    ## No High School Degree              NA         NA 37.23324
    ## Professional Degree                NA         NA 46.30972
    ## Some College                       NA         NA 37.83410

``` r
tapply(LMA.Data$Usual.Weekly.Hours, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), 
    sd)
```

    ##                           Asian Biracial    Black Hawaiian  Hispanic
    ## Associates Degree     12.736051       NA 10.36064       NA 10.224010
    ## Bachelors Degree      10.918057       NA 10.48933       NA 10.123978
    ## Doctorate              9.746043       NA 10.60228       NA  8.819356
    ## High School Degree    12.045957       NA 10.34333       NA  9.860018
    ## Masters Degree         9.186683       NA  8.56871       NA 11.060748
    ## No High School Degree 11.203396       NA 10.96460       NA 10.415615
    ## Professional Degree   11.990309       NA 15.29519       NA 13.033442
    ## Some College          14.419977       NA 12.27363       NA 11.878227
    ##                       Native American Other Race    White
    ## Associates Degree                  NA         NA 11.22268
    ## Bachelors Degree                   NA         NA 11.39129
    ## Doctorate                          NA         NA 12.90807
    ## High School Degree                 NA         NA 11.78901
    ## Masters Degree                     NA         NA 11.64413
    ## No High School Degree              NA         NA 13.25812
    ## Professional Degree                NA         NA 13.48702
    ## Some College                       NA         NA 12.57262

``` r
tapply(LMA.Data$Age, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), 
    mean)
```

    ##                          Asian Biracial    Black Hawaiian Hispanic
    ## Associates Degree     42.22360       NA 41.91803       NA 38.54545
    ## Bachelors Degree      40.23156       NA 41.35880       NA 38.14943
    ## Doctorate             44.34066       NA 45.34286       NA 47.33333
    ## High School Degree    41.54808       NA 41.41215       NA 36.31317
    ## Masters Degree        40.47748       NA 44.08711       NA 42.37387
    ## No High School Degree 45.15278       NA 41.66173       NA 39.61083
    ## Professional Degree   42.00000       NA 44.63889       NA 42.04054
    ## Some College          36.88920       NA 37.70941       NA 34.10209
    ##                       Native American Other Race    White
    ## Associates Degree                  NA         NA 43.18615
    ## Bachelors Degree                   NA         NA 41.92747
    ## Doctorate                          NA         NA 46.87991
    ## High School Degree                 NA         NA 43.01674
    ## Masters Degree                     NA         NA 44.93633
    ## No High School Degree              NA         NA 40.72709
    ## Professional Degree                NA         NA 47.09306
    ## Some College                       NA         NA 39.67576

``` r
tapply(LMA.Data$Age, list(LMA.Data$Educational.Attainment, LMA.Data$Race.Ethnicity), 
    sd)
```

    ##                           Asian Biracial    Black Hawaiian  Hispanic
    ## Associates Degree     11.766465       NA 11.36150       NA 11.572257
    ## Bachelors Degree      11.591308       NA 11.67891       NA 11.211000
    ## Doctorate              9.960165       NA 10.70718       NA  9.815953
    ## High School Degree    12.554867       NA 12.73770       NA 12.204731
    ## Masters Degree        10.131824       NA 10.59041       NA 10.384458
    ## No High School Degree 10.986241       NA 14.03961       NA 11.887667
    ## Professional Degree    9.428207       NA 10.17884       NA 10.085170
    ## Some College          14.158512       NA 13.44597       NA 12.376248
    ##                       Native American Other Race    White
    ## Associates Degree                  NA         NA 11.93426
    ## Bachelors Degree                   NA         NA 12.07961
    ## Doctorate                          NA         NA 10.44028
    ## High School Degree                 NA         NA 13.38698
    ## Masters Degree                     NA         NA 11.29225
    ## No High School Degree              NA         NA 14.24404
    ## Professional Degree                NA         NA 10.87435
    ## Some College                       NA         NA 14.33905

### Preliminary Model - Remove Weekly hours

``` r
# Estimate MR Model
Earnings.Equation = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + Female + 
    Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + 
    Some.College + High.School.Degree.or.GED + No.High.School.Degree + Black + 
    Asian + Hispanic, data = LMA.Data)
summary(Earnings.Equation)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    ##     Professional.Degree + Some.College + High.School.Degree.or.GED + 
    ##     No.High.School.Degree + Black + Asian + Hispanic, data = LMA.Data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2792 -0.4163  0.1317  0.5610  3.5943 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                6.732e+00  4.636e-02 145.228  < 2e-16 ***
    ## Age                        1.653e-01  2.309e-03  71.602  < 2e-16 ***
    ## I(Age * Age)              -1.730e-03  2.768e-05 -62.486  < 2e-16 ***
    ## Female                    -3.802e-01  8.287e-03 -45.884  < 2e-16 ***
    ## Married                    1.855e-01  9.222e-03  20.113  < 2e-16 ***
    ## Bachelors.Degree           3.189e-01  1.648e-02  19.346  < 2e-16 ***
    ## Masters.Degree             5.094e-01  1.973e-02  25.821  < 2e-16 ***
    ## Doctorate                  6.719e-01  3.835e-02  17.518  < 2e-16 ***
    ## Professional.Degree        9.053e-01  3.133e-02  28.896  < 2e-16 ***
    ## Some.College              -1.709e-01  1.615e-02 -10.585  < 2e-16 ***
    ## High.School.Degree.or.GED -2.731e-01  1.602e-02 -17.046  < 2e-16 ***
    ## No.High.School.Degree     -5.839e-01  2.029e-02 -28.778  < 2e-16 ***
    ## Black                     -1.168e-01  1.419e-02  -8.230  < 2e-16 ***
    ## Asian                     -1.446e-02  1.888e-02  -0.766 0.443755    
    ## Hispanic                  -4.482e-02  1.291e-02  -3.472 0.000518 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8575 on 43607 degrees of freedom
    ## Multiple R-squared:  0.3624, Adjusted R-squared:  0.3622 
    ## F-statistic:  1771 on 14 and 43607 DF,  p-value: < 2.2e-16

``` r
bptest(Earnings.Equation)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  Earnings.Equation
    ## BP = 264.01, df = 14, p-value < 2.2e-16

``` r
bptestequation = lm(residuals(Earnings.Equation) * residuals(Earnings.Equation) ~ 
    Age + I(Age * Age) + Female + Married + Bachelors.Degree + Masters.Degree + 
        Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + 
        No.High.School.Degree + Black + Asian + Hispanic, data = LMA.Data)
summary(bptestequation)
```

    ## 
    ## Call:
    ## lm(formula = residuals(Earnings.Equation) * residuals(Earnings.Equation) ~ 
    ##     Age + I(Age * Age) + Female + Married + Bachelors.Degree + 
    ##         Masters.Degree + Doctorate + Professional.Degree + Some.College + 
    ##         High.School.Degree.or.GED + No.High.School.Degree + Black + 
    ##         Asian + Hispanic, data = LMA.Data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2367 -0.6416 -0.4566  0.0491 17.1535 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.302e+00  7.224e-02  18.028  < 2e-16 ***
    ## Age                       -3.182e-02  3.597e-03  -8.846  < 2e-16 ***
    ## I(Age * Age)               4.135e-04  4.313e-05   9.588  < 2e-16 ***
    ## Female                     1.845e-02  1.291e-02   1.429 0.153092    
    ## Married                   -1.025e-01  1.437e-02  -7.135 9.84e-13 ***
    ## Bachelors.Degree           9.040e-02  2.569e-02   3.520 0.000433 ***
    ## Masters.Degree             4.890e-02  3.074e-02   1.591 0.111690    
    ## Doctorate                 -1.226e-02  5.976e-02  -0.205 0.837487    
    ## Professional.Degree        2.941e-01  4.882e-02   6.023 1.72e-09 ***
    ## Some.College               2.579e-02  2.517e-02   1.025 0.305564    
    ## High.School.Degree.or.GED -2.790e-02  2.497e-02  -1.117 0.263844    
    ## No.High.School.Degree      3.272e-03  3.162e-02   0.104 0.917567    
    ## Black                     -1.842e-02  2.212e-02  -0.833 0.404973    
    ## Asian                      4.976e-03  2.942e-02   0.169 0.865703    
    ## Hispanic                  -4.736e-02  2.012e-02  -2.354 0.018552 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.336 on 43607 degrees of freedom
    ## Multiple R-squared:  0.006052,   Adjusted R-squared:  0.005733 
    ## F-statistic: 18.97 on 14 and 43607 DF,  p-value: < 2.2e-16

``` r
(sandwich_se1 <- diag(vcovHC(Earnings.Equation, type = "HC"))^0.5)
```

    ##               (Intercept)                       Age 
    ##              4.844452e-02              2.428383e-03 
    ##              I(Age * Age)                    Female 
    ##              2.928067e-05              8.317977e-03 
    ##                   Married          Bachelors.Degree 
    ##              9.282590e-03              1.636596e-02 
    ##            Masters.Degree                 Doctorate 
    ##              1.949040e-02              3.698366e-02 
    ##       Professional.Degree              Some.College 
    ##              3.523586e-02              1.587847e-02 
    ## High.School.Degree.or.GED     No.High.School.Degree 
    ##              1.557693e-02              1.986169e-02 
    ##                     Black                     Asian 
    ##              1.417447e-02              1.904177e-02 
    ##                  Hispanic 
    ##              1.258784e-02

``` r
(exp(-0.3802) - 1) * 100
```

    ## [1] -31.62753

``` r
(exp(0.1855) - 1) * 100
```

    ## [1] 20.38202

``` r
(exp(-0.5839) - 1) * 100
```

    ## [1] -44.2281

``` r
(0.1653 - (2 * 0.00173 * 25)) * 100
```

    ## [1] 7.88

``` r
(0.1653 - (2 * 0.00173 * 55)) * 100
```

    ## [1] -2.5

``` r
(exp(-0.2593) - 1) * 100  #-48.25367 Asian
```

    ## [1] -22.84085

``` r
(exp(-0.2739) - 1) * 100  #-43.41917 Black
```

    ## [1] -23.95919

``` r
(exp(-0.3543233) - 1) * 100  #-41.61435 White
```

    ## [1] -29.83519

``` r
(exp(-0.3539) - 1) * 100  #-48.54693 Hispanic
```

    ## [1] -29.80548

### Model Estimation

``` r
# Estimate MR Model for Race = White
Earnings.Equation.White = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + 
    Some.College + High.School.Degree.or.GED + No.High.School.Degree, data = subset(LMA.Data, 
    Race.Ethnicity == "White"))
summary(Earnings.Equation.White)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    ##     Professional.Degree + Some.College + High.School.Degree.or.GED + 
    ##     No.High.School.Degree, data = subset(LMA.Data, Race.Ethnicity == 
    ##     "White"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3071 -0.4077  0.1290  0.5593  3.6644 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                6.563e+00  5.520e-02 118.884   <2e-16 ***
    ## Age                        1.730e-01  2.743e-03  63.068   <2e-16 ***
    ## I(Age * Age)              -1.811e-03  3.266e-05 -55.442   <2e-16 ***
    ## Female                    -4.288e-01  9.814e-03 -43.698   <2e-16 ***
    ## Married                    2.072e-01  1.098e-02  18.864   <2e-16 ***
    ## Bachelors.Degree           3.374e-01  1.880e-02  17.950   <2e-16 ***
    ## Masters.Degree             5.074e-01  2.256e-02  22.485   <2e-16 ***
    ## Doctorate                  6.456e-01  4.438e-02  14.545   <2e-16 ***
    ## Professional.Degree        9.302e-01  3.584e-02  25.951   <2e-16 ***
    ## Some.College              -1.567e-01  1.866e-02  -8.401   <2e-16 ***
    ## High.School.Degree.or.GED -2.593e-01  1.851e-02 -14.008   <2e-16 ***
    ## No.High.School.Degree     -5.381e-01  2.764e-02 -19.467   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8629 on 31278 degrees of freedom
    ## Multiple R-squared:  0.3667, Adjusted R-squared:  0.3664 
    ## F-statistic:  1646 on 11 and 31278 DF,  p-value: < 2.2e-16

``` r
# Estimate MR Model for Race = Black
Earnings.Equation.Black = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + 
    Some.College + High.School.Degree.or.GED + No.High.School.Degree, data = subset(LMA.Data, 
    Race.Ethnicity == "Black"))
summary(Earnings.Equation.Black)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    ##     Professional.Degree + Some.College + High.School.Degree.or.GED + 
    ##     No.High.School.Degree, data = subset(LMA.Data, Race.Ethnicity == 
    ##     "Black"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.4360 -0.4377  0.1454  0.5772  3.2821 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                6.644e+00  1.456e-01  45.635  < 2e-16 ***
    ## Age                        1.578e-01  7.153e-03  22.057  < 2e-16 ***
    ## I(Age * Age)              -1.646e-03  8.721e-05 -18.879  < 2e-16 ***
    ## Female                    -1.719e-01  2.663e-02  -6.456 1.19e-10 ***
    ## Married                    1.948e-01  2.873e-02   6.779 1.38e-11 ***
    ## Bachelors.Degree           2.611e-01  5.642e-02   4.627 3.81e-06 ***
    ## Masters.Degree             6.222e-01  6.714e-02   9.267  < 2e-16 ***
    ## Doctorate                  7.748e-01  1.506e-01   5.144 2.80e-07 ***
    ## Professional.Degree        7.816e-01  1.487e-01   5.256 1.55e-07 ***
    ## Some.College              -1.545e-01  5.074e-02  -3.045  0.00234 ** 
    ## High.School.Degree.or.GED -2.739e-01  5.092e-02  -5.380 7.86e-08 ***
    ## No.High.School.Degree     -5.695e-01  6.206e-02  -9.177  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8502 on 4272 degrees of freedom
    ## Multiple R-squared:  0.319,  Adjusted R-squared:  0.3173 
    ## F-statistic: 181.9 on 11 and 4272 DF,  p-value: < 2.2e-16

``` r
# Estimate MR Model for Race = Asian
Earnings.Equation.Asian = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + Professional.Degree + 
    Some.College + High.School.Degree.or.GED + No.High.School.Degree, data = subset(LMA.Data, 
    Race.Ethnicity == "Asian"))
summary(Earnings.Equation.Asian)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    ##     Professional.Degree + Some.College + High.School.Degree.or.GED + 
    ##     No.High.School.Degree, data = subset(LMA.Data, Race.Ethnicity == 
    ##     "Asian"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.1919 -0.4781  0.1194  0.5685  3.1873 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                6.1193045  0.2338809  26.164  < 2e-16 ***
    ## Age                        0.1954111  0.0116070  16.836  < 2e-16 ***
    ## I(Age * Age)              -0.0020931  0.0001373 -15.240  < 2e-16 ***
    ## Female                    -0.2492512  0.0365840  -6.813 1.22e-11 ***
    ## Married                    0.0881647  0.0444709   1.983  0.04754 *  
    ## Bachelors.Degree           0.4131178  0.0750935   5.501 4.20e-08 ***
    ## Masters.Degree             0.6081632  0.0825511   7.367 2.44e-13 ***
    ## Doctorate                  0.8612628  0.1126429   7.646 3.06e-14 ***
    ## Professional.Degree        1.1239604  0.1085375  10.356  < 2e-16 ***
    ## Some.College              -0.2706511  0.0824703  -3.282  0.00105 ** 
    ## High.School.Degree.or.GED -0.3543233  0.0831027  -4.264 2.09e-05 ***
    ## No.High.School.Degree     -0.6588167  0.0893045  -7.377 2.27e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8556 on 2233 degrees of freedom
    ## Multiple R-squared:  0.4169, Adjusted R-squared:  0.414 
    ## F-statistic: 145.1 on 11 and 2233 DF,  p-value: < 2.2e-16

``` r
# Estimate MR Model for Race = Hispanic
Earnings.Equation.Hispanic = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * 
    Age) + Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    Professional.Degree + Some.College + High.School.Degree.or.GED + No.High.School.Degree, 
    data = subset(LMA.Data, Race.Ethnicity == "Hispanic"))
summary(Earnings.Equation.Hispanic)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + Bachelors.Degree + Masters.Degree + Doctorate + 
    ##     Professional.Degree + Some.College + High.School.Degree.or.GED + 
    ##     No.High.School.Degree, data = subset(LMA.Data, Race.Ethnicity == 
    ##     "Hispanic"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.6000 -0.4097  0.1242  0.5335  2.5827 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                7.536e+00  1.206e-01  62.510  < 2e-16 ***
    ## Age                        1.295e-01  6.090e-03  21.268  < 2e-16 ***
    ## I(Age * Age)              -1.345e-03  7.573e-05 -17.760  < 2e-16 ***
    ## Female                    -3.067e-01  2.182e-02 -14.056  < 2e-16 ***
    ## Married                    1.079e-01  2.354e-02   4.585 4.63e-06 ***
    ## Bachelors.Degree           1.176e-01  5.379e-02   2.186   0.0289 *  
    ## Masters.Degree             3.379e-01  6.923e-02   4.880 1.09e-06 ***
    ## Doctorate                  3.814e-01  1.960e-01   1.946   0.0517 .  
    ## Professional.Degree        4.215e-01  1.035e-01   4.074 4.69e-05 ***
    ## Some.College              -2.536e-01  4.825e-02  -5.256 1.53e-07 ***
    ## High.School.Degree.or.GED -3.539e-01  4.734e-02  -7.476 8.83e-14 ***
    ## No.High.School.Degree     -6.645e-01  4.742e-02 -14.014  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8106 on 5791 degrees of freedom
    ## Multiple R-squared:  0.272,  Adjusted R-squared:  0.2707 
    ## F-statistic: 196.7 on 11 and 5791 DF,  p-value: < 2.2e-16

``` r
# Full Model

Earnings.EquationF = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + Race.Ethnicity * Educational.Attainment, data = LMA.Data)
# summary(Earnings.Equation)

round(summary(Earnings.EquationF)$coefficients[, 1:2], 7)
```

    ##                                                                      Estimate
    ## (Intercept)                                                         6.6942524
    ## Age                                                                 0.1654655
    ## I(Age * Age)                                                       -0.0017308
    ## Female                                                             -0.3789804
    ## Married                                                             0.1860314
    ## Race.EthnicityBlack                                                -0.0678652
    ## Race.EthnicityHispanic                                              0.0821475
    ## Race.EthnicityWhite                                                 0.0196985
    ## Educational.AttainmentBachelors Degree                              0.4122047
    ## Educational.AttainmentDoctorate                                     0.8294855
    ## Educational.AttainmentHigh School Degree                           -0.3667453
    ## Educational.AttainmentMasters Degree                                0.5945689
    ## Educational.AttainmentNo High School Degree                        -0.6642265
    ## Educational.AttainmentProfessional Degree                           1.1238849
    ## Educational.AttainmentSome College                                 -0.2886776
    ## Race.EthnicityBlack:Educational.AttainmentBachelors Degree         -0.1648520
    ## Race.EthnicityHispanic:Educational.AttainmentBachelors Degree      -0.3006408
    ## Race.EthnicityWhite:Educational.AttainmentBachelors Degree         -0.0734701
    ## Race.EthnicityBlack:Educational.AttainmentDoctorate                -0.0800870
    ## Race.EthnicityHispanic:Educational.AttainmentDoctorate             -0.5102847
    ## Race.EthnicityWhite:Educational.AttainmentDoctorate                -0.1716397
    ## Race.EthnicityBlack:Educational.AttainmentHigh School Degree        0.0582416
    ## Race.EthnicityHispanic:Educational.AttainmentHigh School Degree     0.0250021
    ## Race.EthnicityWhite:Educational.AttainmentHigh School Degree        0.1085442
    ## Race.EthnicityBlack:Educational.AttainmentMasters Degree            0.0188134
    ## Race.EthnicityHispanic:Educational.AttainmentMasters Degree        -0.2997579
    ## Race.EthnicityWhite:Educational.AttainmentMasters Degree           -0.0836049
    ## Race.EthnicityBlack:Educational.AttainmentNo High School Degree     0.0456112
    ## Race.EthnicityHispanic:Educational.AttainmentNo High School Degree -0.0248702
    ## Race.EthnicityWhite:Educational.AttainmentNo High School Degree     0.1247716
    ## Race.EthnicityBlack:Educational.AttainmentProfessional Degree      -0.3894357
    ## Race.EthnicityHispanic:Educational.AttainmentProfessional Degree   -0.7481903
    ## Race.EthnicityWhite:Educational.AttainmentProfessional Degree      -0.1823396
    ## Race.EthnicityBlack:Educational.AttainmentSome College              0.1206925
    ## Race.EthnicityHispanic:Educational.AttainmentSome College           0.0797411
    ## Race.EthnicityWhite:Educational.AttainmentSome College              0.1239339
    ##                                                                    Std. Error
    ## (Intercept)                                                         0.0807985
    ## Age                                                                 0.0023092
    ## I(Age * Age)                                                        0.0000277
    ## Female                                                              0.0082890
    ## Married                                                             0.0092207
    ## Race.EthnicityBlack                                                 0.0810607
    ## Race.EthnicityHispanic                                              0.0811365
    ## Race.EthnicityWhite                                                 0.0692984
    ## Educational.AttainmentBachelors Degree                              0.0751132
    ## Educational.AttainmentDoctorate                                     0.1123825
    ## Educational.AttainmentHigh School Degree                            0.0831379
    ## Educational.AttainmentMasters Degree                                0.0822577
    ## Educational.AttainmentNo High School Degree                         0.0892122
    ## Educational.AttainmentProfessional Degree                           0.1084319
    ## Educational.AttainmentSome College                                  0.0815550
    ## Race.EthnicityBlack:Educational.AttainmentBachelors Degree          0.0941640
    ## Race.EthnicityHispanic:Educational.AttainmentBachelors Degree       0.0941769
    ## Race.EthnicityWhite:Educational.AttainmentBachelors Degree          0.0773925
    ## Race.EthnicityBlack:Educational.AttainmentDoctorate                 0.1886892
    ## Race.EthnicityHispanic:Educational.AttainmentDoctorate              0.2354366
    ## Race.EthnicityWhite:Educational.AttainmentDoctorate                 0.1206839
    ## Race.EthnicityBlack:Educational.AttainmentHigh School Degree        0.0975362
    ## Race.EthnicityHispanic:Educational.AttainmentHigh School Degree     0.0969389
    ## Race.EthnicityWhite:Educational.AttainmentHigh School Degree        0.0851338
    ## Race.EthnicityBlack:Educational.AttainmentMasters Degree            0.1064358
    ## Race.EthnicityHispanic:Educational.AttainmentMasters Degree         0.1099797
    ## Race.EthnicityWhite:Educational.AttainmentMasters Degree            0.0852506
    ## Race.EthnicityBlack:Educational.AttainmentNo High School Degree     0.1085730
    ## Race.EthnicityHispanic:Educational.AttainmentNo High School Degree  0.1022076
    ## Race.EthnicityWhite:Educational.AttainmentNo High School Degree     0.0933488
    ## Race.EthnicityBlack:Educational.AttainmentProfessional Degree       0.1848049
    ## Race.EthnicityHispanic:Educational.AttainmentProfessional Degree    0.1539395
    ## Race.EthnicityWhite:Educational.AttainmentProfessional Degree       0.1141161
    ## Race.EthnicityBlack:Educational.AttainmentSome College              0.0959547
    ## Race.EthnicityHispanic:Educational.AttainmentSome College           0.0959872
    ## Race.EthnicityWhite:Educational.AttainmentSome College              0.0835589

``` r
Earnings.EquationF1 = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + Race.Ethnicity * Educational.Attainment, data = subset(LMA.Data, 
    Race.Ethnicity %in% c("White", "Black")))

round(summary(Earnings.EquationF1)$coefficients[, 1:2], 7)
```

    ##                                                                   Estimate
    ## (Intercept)                                                      6.5056506
    ## Age                                                              0.1712159
    ## I(Age * Age)                                                    -0.0017913
    ## Female                                                          -0.3988205
    ## Married                                                          0.2051427
    ## Race.EthnicityWhite                                              0.0800952
    ## Educational.AttainmentBachelors Degree                           0.2457254
    ## Educational.AttainmentDoctorate                                  0.7398665
    ## Educational.AttainmentHigh School Degree                        -0.3088533
    ## Educational.AttainmentMasters Degree                             0.6080500
    ## Educational.AttainmentNo High School Degree                     -0.6175691
    ## Educational.AttainmentProfessional Degree                        0.7233849
    ## Educational.AttainmentSome College                              -0.1612200
    ## Race.EthnicityWhite:Educational.AttainmentBachelors Degree       0.0929855
    ## Race.EthnicityWhite:Educational.AttainmentDoctorate             -0.0892615
    ## Race.EthnicityWhite:Educational.AttainmentHigh School Degree     0.0521067
    ## Race.EthnicityWhite:Educational.AttainmentMasters Degree        -0.1000463
    ## Race.EthnicityWhite:Educational.AttainmentNo High School Degree  0.0829035
    ## Race.EthnicityWhite:Educational.AttainmentProfessional Degree    0.2114020
    ## Race.EthnicityWhite:Educational.AttainmentSome College           0.0041591
    ##                                                                 Std. Error
    ## (Intercept)                                                      0.0675734
    ## Age                                                              0.0025605
    ## I(Age * Age)                                                     0.0000306
    ## Female                                                           0.0092180
    ## Married                                                          0.0102578
    ## Race.EthnicityWhite                                              0.0478169
    ## Educational.AttainmentBachelors Degree                           0.0571677
    ## Educational.AttainmentDoctorate                                  0.1525975
    ## Educational.AttainmentHigh School Degree                         0.0513606
    ## Educational.AttainmentMasters Degree                             0.0680052
    ## Educational.AttainmentNo High School Degree                      0.0622787
    ## Educational.AttainmentProfessional Degree                        0.1506537
    ## Educational.AttainmentSome College                               0.0510161
    ## Race.EthnicityWhite:Educational.AttainmentBachelors Degree       0.0601675
    ## Race.EthnicityWhite:Educational.AttainmentDoctorate              0.1588912
    ## Race.EthnicityWhite:Educational.AttainmentHigh School Degree     0.0545323
    ## Race.EthnicityWhite:Educational.AttainmentMasters Degree         0.0716335
    ## Race.EthnicityWhite:Educational.AttainmentNo High School Degree  0.0680042
    ## Race.EthnicityWhite:Educational.AttainmentProfessional Degree    0.1548255
    ## Race.EthnicityWhite:Educational.AttainmentSome College           0.0542048

``` r
Earnings.EquationF2 = lm(log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    Female + Married + White * No.High.School.Degree + White * High.School.Degree.or.GED + 
    White * Some.College + White * Bachelors.Degree + White * Masters.Degree + 
    White * Professional.Degree + White * Doctorate + White * Associates.Degree, 
    data = subset(LMA.Data, Race.Ethnicity %in% c("White", "Black")))

summary(Earnings.EquationF2)
```

    ## 
    ## Call:
    ## lm(formula = log(Earnings.Past.12.Months) ~ Age + I(Age * Age) + 
    ##     Female + Married + White * No.High.School.Degree + White * 
    ##     High.School.Degree.or.GED + White * Some.College + White * 
    ##     Bachelors.Degree + White * Masters.Degree + White * Professional.Degree + 
    ##     White * Doctorate + White * Associates.Degree, data = subset(LMA.Data, 
    ##     Race.Ethnicity %in% c("White", "Black")))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.2966 -0.4107  0.1335  0.5608  3.6397 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                      6.506e+00  6.757e-02  96.275  < 2e-16 ***
    ## Age                              1.712e-01  2.560e-03  66.869  < 2e-16 ***
    ## I(Age * Age)                    -1.791e-03  3.057e-05 -58.593  < 2e-16 ***
    ## Female                          -3.988e-01  9.218e-03 -43.265  < 2e-16 ***
    ## Married                          2.051e-01  1.026e-02  19.999  < 2e-16 ***
    ## White                            8.010e-02  4.782e-02   1.675  0.09394 .  
    ## No.High.School.Degree           -6.176e-01  6.228e-02  -9.916  < 2e-16 ***
    ## High.School.Degree.or.GED       -3.089e-01  5.136e-02  -6.013 1.83e-09 ***
    ## Some.College                    -1.612e-01  5.102e-02  -3.160  0.00158 ** 
    ## Bachelors.Degree                 2.457e-01  5.717e-02   4.298 1.73e-05 ***
    ## Masters.Degree                   6.081e-01  6.801e-02   8.941  < 2e-16 ***
    ## Professional.Degree              7.234e-01  1.507e-01   4.802 1.58e-06 ***
    ## Doctorate                        7.399e-01  1.526e-01   4.848 1.25e-06 ***
    ## Associates.Degree                       NA         NA      NA       NA    
    ## White:No.High.School.Degree      8.290e-02  6.800e-02   1.219  0.22282    
    ## White:High.School.Degree.or.GED  5.211e-02  5.453e-02   0.956  0.33932    
    ## White:Some.College               4.159e-03  5.420e-02   0.077  0.93884    
    ## White:Bachelors.Degree           9.299e-02  6.017e-02   1.545  0.12225    
    ## White:Masters.Degree            -1.000e-01  7.163e-02  -1.397  0.16253    
    ## White:Professional.Degree        2.114e-01  1.548e-01   1.365  0.17213    
    ## White:Doctorate                 -8.926e-02  1.589e-01  -0.562  0.57427    
    ## White:Associates.Degree                 NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8624 on 35554 degrees of freedom
    ## Multiple R-squared:  0.3656, Adjusted R-squared:  0.3653 
    ## F-statistic:  1078 on 19 and 35554 DF,  p-value: < 2.2e-16

``` r
round(summary(Earnings.EquationF2)$coefficients[, 1:2], 7)
```

    ##                                   Estimate Std. Error
    ## (Intercept)                      6.5056506  0.0675734
    ## Age                              0.1712159  0.0025605
    ## I(Age * Age)                    -0.0017913  0.0000306
    ## Female                          -0.3988205  0.0092180
    ## Married                          0.2051427  0.0102578
    ## White                            0.0800952  0.0478169
    ## No.High.School.Degree           -0.6175691  0.0622787
    ## High.School.Degree.or.GED       -0.3088533  0.0513606
    ## Some.College                    -0.1612200  0.0510161
    ## Bachelors.Degree                 0.2457254  0.0571677
    ## Masters.Degree                   0.6080500  0.0680052
    ## Professional.Degree              0.7233849  0.1506537
    ## Doctorate                        0.7398665  0.1525975
    ## White:No.High.School.Degree      0.0829035  0.0680042
    ## White:High.School.Degree.or.GED  0.0521067  0.0545323
    ## White:Some.College               0.0041591  0.0542048
    ## White:Bachelors.Degree           0.0929855  0.0601675
    ## White:Masters.Degree            -0.1000463  0.0716335
    ## White:Professional.Degree        0.2114020  0.1548255
    ## White:Doctorate                 -0.0892615  0.1588912

``` r
(exp(0.09299 + 0.0801) - 1) * 100  #18.897
```

    ## [1] 18.89731

### Perform BPG Test

``` r
bptest(Earnings.Equation.White)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  Earnings.Equation.White
    ## BP = 177.2, df = 11, p-value < 2.2e-16

``` r
bptestequationwhite = lm(residuals(Earnings.Equation.White) * residuals(Earnings.Equation.White) ~ 
    Age + I(Age * Age) + Female + Married + Bachelors.Degree + Masters.Degree + 
        Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + 
        No.High.School.Degree, data = subset(LMA.Data, Race.Ethnicity == "White"))
summary(bptestequationwhite)
```

    ## 
    ## Call:
    ## lm(formula = residuals(Earnings.Equation.White) * residuals(Earnings.Equation.White) ~ 
    ##     Age + I(Age * Age) + Female + Married + Bachelors.Degree + 
    ##         Masters.Degree + Doctorate + Professional.Degree + Some.College + 
    ##         High.School.Degree.or.GED + No.High.School.Degree, data = subset(LMA.Data, 
    ##     Race.Ethnicity == "White"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2143 -0.6521 -0.4720  0.0326 17.4191 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.189e+00  8.827e-02  13.470  < 2e-16 ***
    ## Age                       -2.716e-02  4.386e-03  -6.191 6.04e-10 ***
    ## I(Age * Age)               3.693e-04  5.222e-05   7.072 1.56e-12 ***
    ## Female                     4.330e-02  1.569e-02   2.759  0.00580 ** 
    ## Married                   -1.177e-01  1.756e-02  -6.702 2.09e-11 ***
    ## Bachelors.Degree           8.807e-02  3.006e-02   2.930  0.00339 ** 
    ## Masters.Degree             5.546e-02  3.608e-02   1.537  0.12430    
    ## Doctorate                  1.440e-02  7.097e-02   0.203  0.83920    
    ## Professional.Degree        2.595e-01  5.731e-02   4.527 5.99e-06 ***
    ## Some.College               1.395e-02  2.983e-02   0.468  0.63996    
    ## High.School.Degree.or.GED -2.316e-02  2.959e-02  -0.782  0.43394    
    ## No.High.School.Degree      1.596e-02  4.420e-02   0.361  0.71796    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.38 on 31278 degrees of freedom
    ## Multiple R-squared:  0.005663,   Adjusted R-squared:  0.005313 
    ## F-statistic: 16.19 on 11 and 31278 DF,  p-value: < 2.2e-16

``` r
bptest(Earnings.Equation.Black)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  Earnings.Equation.Black
    ## BP = 35.54, df = 11, p-value = 0.0002018

``` r
bptestequationblack = lm(residuals(Earnings.Equation.Black) * residuals(Earnings.Equation.Black) ~ 
    Age + I(Age * Age) + Female + Married + Bachelors.Degree + Masters.Degree + 
        Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + 
        No.High.School.Degree + Black, data = subset(LMA.Data, Race.Ethnicity == 
    "Black"))
summary(bptestequationblack)
```

    ## 
    ## Call:
    ## lm(formula = residuals(Earnings.Equation.Black) * residuals(Earnings.Equation.Black) ~ 
    ##     Age + I(Age * Age) + Female + Married + Bachelors.Degree + 
    ##         Masters.Degree + Doctorate + Professional.Degree + Some.College + 
    ##         High.School.Degree.or.GED + No.High.School.Degree + Black, 
    ##     data = subset(LMA.Data, Race.Ethnicity == "Black"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.2074 -0.6184 -0.4187  0.0906 11.3169 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.3462375  0.2084273   6.459 1.17e-10 ***
    ## Age                       -0.0310613  0.0102408  -3.033  0.00244 ** 
    ## I(Age * Age)               0.0003733  0.0001249   2.990  0.00281 ** 
    ## Female                    -0.0576606  0.0381306  -1.512  0.13056    
    ## Married                   -0.0995222  0.0411326  -2.420  0.01558 *  
    ## Bachelors.Degree           0.0674313  0.0807705   0.835  0.40385    
    ## Masters.Degree            -0.0751244  0.0961278  -0.782  0.43455    
    ## Doctorate                 -0.1005032  0.2156257  -0.466  0.64117    
    ## Professional.Degree        0.4869252  0.2129084   2.287  0.02224 *  
    ## Some.College               0.0554821  0.0726390   0.764  0.44502    
    ## High.School.Degree.or.GED -0.0210050  0.0729049  -0.288  0.77327    
    ## No.High.School.Degree      0.0955409  0.0888454   1.075  0.28227    
    ## Black                             NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.217 on 4272 degrees of freedom
    ## Multiple R-squared:  0.008296,   Adjusted R-squared:  0.005742 
    ## F-statistic: 3.249 on 11 and 4272 DF,  p-value: 0.0001953

``` r
bptest(Earnings.Equation.Asian)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  Earnings.Equation.Asian
    ## BP = 26.606, df = 11, p-value = 0.005268

``` r
bptestequationasian = lm(residuals(Earnings.Equation.Asian) * residuals(Earnings.Equation.Asian) ~ 
    Age + I(Age * Age) + Female + Married + Bachelors.Degree + Masters.Degree + 
        Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + 
        No.High.School.Degree + Asian, data = subset(LMA.Data, Race.Ethnicity == 
    "Asian"))
summary(bptestequationasian)
```

    ## 
    ## Call:
    ## lm(formula = residuals(Earnings.Equation.Asian) * residuals(Earnings.Equation.Asian) ~ 
    ##     Age + I(Age * Age) + Female + Married + Bachelors.Degree + 
    ##         Masters.Degree + Doctorate + Professional.Degree + Some.College + 
    ##         High.School.Degree.or.GED + No.High.School.Degree + Asian, 
    ##     data = subset(LMA.Data, Race.Ethnicity == "Asian"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0477 -0.6153 -0.4105  0.0717 16.8587 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.8734323  0.3563075   5.258  1.6e-07 ***
    ## Age                       -0.0592335  0.0176827  -3.350 0.000822 ***
    ## I(Age * Age)               0.0007071  0.0002092   3.379 0.000740 ***
    ## Female                    -0.0346754  0.0557342  -0.622 0.533902    
    ## Married                   -0.0605224  0.0677496  -0.893 0.371779    
    ## Bachelors.Degree           0.1178684  0.1144017   1.030 0.302979    
    ## Masters.Degree             0.0778770  0.1257631   0.619 0.535824    
    ## Doctorate                 -0.0487143  0.1716065  -0.284 0.776535    
    ## Professional.Degree        0.1763674  0.1653522   1.067 0.286260    
    ## Some.College               0.0855861  0.1256399   0.681 0.495814    
    ## High.School.Degree.or.GED -0.1132784  0.1266033  -0.895 0.371017    
    ## No.High.School.Degree     -0.0130869  0.1360515  -0.096 0.923378    
    ## Asian                             NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.304 on 2233 degrees of freedom
    ## Multiple R-squared:  0.01185,    Adjusted R-squared:  0.006984 
    ## F-statistic: 2.435 on 11 and 2233 DF,  p-value: 0.005143

``` r
bptest(Earnings.Equation.Hispanic)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  Earnings.Equation.Hispanic
    ## BP = 45.495, df = 11, p-value = 3.969e-06

``` r
bptestequationhispanic = lm(residuals(Earnings.Equation.Hispanic) * residuals(Earnings.Equation.Hispanic) ~ 
    Age + I(Age * Age) + Female + Married + Bachelors.Degree + Masters.Degree + 
        Doctorate + Professional.Degree + Some.College + High.School.Degree.or.GED + 
        No.High.School.Degree + Hispanic, data = subset(LMA.Data, Race.Ethnicity == 
    "Hispanic"))
summary(bptestequationhispanic)
```

    ## 
    ## Call:
    ## lm(formula = residuals(Earnings.Equation.Hispanic) * residuals(Earnings.Equation.Hispanic) ~ 
    ##     Age + I(Age * Age) + Female + Married + Bachelors.Degree + 
    ##         Masters.Degree + Doctorate + Professional.Degree + Some.College + 
    ##         High.School.Degree.or.GED + No.High.School.Degree + Hispanic, 
    ##     data = subset(LMA.Data, Race.Ethnicity == "Hispanic"))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.0785 -0.5643 -0.3983  0.0569 12.2269 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                1.3507861  0.1716541   7.869 4.23e-15 ***
    ## Age                       -0.0351928  0.0086711  -4.059 5.00e-05 ***
    ## I(Age * Age)               0.0004349  0.0001078   4.034 5.56e-05 ***
    ## Female                    -0.0011428  0.0310705  -0.037  0.97066    
    ## Married                   -0.0552560  0.0335126  -1.649  0.09924 .  
    ## Bachelors.Degree           0.0872858  0.0765944   1.140  0.25451    
    ## Masters.Degree             0.0595770  0.0985809   0.604  0.54564    
    ## Doctorate                 -0.0141417  0.2790480  -0.051  0.95958    
    ## Professional.Degree        0.3822053  0.1473481   2.594  0.00951 ** 
    ## Some.College              -0.0014699  0.0687046  -0.021  0.98293    
    ## High.School.Degree.or.GED -0.0642550  0.0674049  -0.953  0.34049    
    ## No.High.School.Degree     -0.0781222  0.0675184  -1.157  0.24730    
    ## Hispanic                          NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.154 on 5791 degrees of freedom
    ## Multiple R-squared:  0.00784,    Adjusted R-squared:  0.005955 
    ## F-statistic:  4.16 on 11 and 5791 DF,  p-value: 3.78e-06

### Correct Standard Errors by generating robust standard errors

``` r
# Generate the Robust standard errors and print them on screen for Whites
(sandwich_se1 <- diag(vcovHC(Earnings.Equation.White, type = "HC"))^0.5)
```

    ##               (Intercept)                       Age 
    ##              5.720621e-02              2.868512e-03 
    ##              I(Age * Age)                    Female 
    ##              3.441388e-05              9.839539e-03 
    ##                   Married          Bachelors.Degree 
    ##              1.114565e-02              1.862388e-02 
    ##            Masters.Degree                 Doctorate 
    ##              2.234456e-02              4.345518e-02 
    ##       Professional.Degree              Some.College 
    ##              3.969286e-02              1.827341e-02 
    ## High.School.Degree.or.GED     No.High.School.Degree 
    ##              1.799771e-02              2.738210e-02

``` r
# Generate the Robust standard errors and print them on screen for Black
(sandwich_se2 <- diag(vcovHC(Earnings.Equation.Black, type = "HC"))^0.5)
```

    ##               (Intercept)                       Age 
    ##              1.520612e-01              7.479888e-03 
    ##              I(Age * Age)                    Female 
    ##              9.104007e-05              2.680848e-02 
    ##                   Married          Bachelors.Degree 
    ##              2.826956e-02              5.529295e-02 
    ##            Masters.Degree                 Doctorate 
    ##              6.190614e-02              1.327696e-01 
    ##       Professional.Degree              Some.College 
    ##              1.835443e-01              4.946521e-02 
    ## High.School.Degree.or.GED     No.High.School.Degree 
    ##              4.890807e-02              6.271687e-02

``` r
# Generate the Robust standard errors and print them on screen for Asian
(sandwich_se3 <- diag(vcovHC(Earnings.Equation.Asian, type = "HC"))^0.5)
```

    ##               (Intercept)                       Age 
    ##              0.2527766245              0.0124456750 
    ##              I(Age * Age)                    Female 
    ##              0.0001474793              0.0371527538 
    ##                   Married          Bachelors.Degree 
    ##              0.0438698634              0.0732533834 
    ##            Masters.Degree                 Doctorate 
    ##              0.0799937685              0.1048007826 
    ##       Professional.Degree              Some.College 
    ##              0.1105328367              0.0820292809 
    ## High.School.Degree.or.GED     No.High.School.Degree 
    ##              0.0775642925              0.0853522016

``` r
# Generate the Robust standard errors and print them on screen for Hispanic
(sandwich_se4 <- diag(vcovHC(Earnings.Equation.Hispanic, type = "HC"))^0.5)
```

    ##               (Intercept)                       Age 
    ##              1.298441e-01              6.526995e-03 
    ##              I(Age * Age)                    Female 
    ##              8.117737e-05              2.203854e-02 
    ##                   Married          Bachelors.Degree 
    ##              2.352729e-02              5.549790e-02 
    ##            Masters.Degree                 Doctorate 
    ##              7.109703e-02              1.957972e-01 
    ##       Professional.Degree              Some.College 
    ##              1.256369e-01              4.906645e-02 
    ## High.School.Degree.or.GED     No.High.School.Degree 
    ##              4.761542e-02              4.750952e-02

### Interpretation of age

``` r
# optimal age for white
0.173/(2 * 0.001811)  #47.76367
```

    ## [1] 47.76367

``` r
(0.173 - (2 * 0.001811 * 30)) * 100  #6.434%
```

    ## [1] 6.434

``` r
(0.173 - (2 * 0.001811 * 50)) * 100  #-0.81%
```

    ## [1] -0.81

``` r
# optimal age for black
0.158/(2 * 0.001646)  #47.99514
```

    ## [1] 47.99514

``` r
(0.158 - (2 * 0.001646 * 30)) * 100  #5.924
```

    ## [1] 5.924

``` r
(0.158 - (2 * 0.001646 * 50)) * 100  #-0.66%
```

    ## [1] -0.66

``` r
# optimal age for asian
0.195411/(2 * 0.002093)  # 46.68204
```

    ## [1] 46.68204

``` r
(0.195411 - (2 * 0.002093 * 30)) * 100  # 6.9831
```

    ## [1] 6.9831

``` r
(0.195411 - (2 * 0.002093 * 50)) * 100  #-1.3889%
```

    ## [1] -1.3889

``` r
# optimal age for hispanic
0.1295/(2 * 0.001345)  # 48.14126
```

    ## [1] 48.14126

``` r
(0.1295 - (2 * 0.001345 * 30)) * 100  #4.88
```

    ## [1] 4.88

``` r
(0.1295 - (2 * 0.001345 * 50)) * 100  #-0.5
```

    ## [1] -0.5

``` r
# White Female
(exp(-0.4288) - 1) * 100  #-34.87098
```

    ## [1] -34.87098

``` r
# Black Female
(exp(-0.1719) - 1) * 100  #-15.79366
```

    ## [1] -15.79366

``` r
# Asian Female
(exp(-0.2492512) - 1) * 100  #-22.06158
```

    ## [1] -22.06158

``` r
# Hispanic Female
(exp(-0.3067) - 1) * 100  #-26.41287
```

    ## [1] -26.41287

``` r
# White Married
(exp(0.2072) - 1) * 100  #23.02286
```

    ## [1] 23.02286

``` r
# Black Married
(exp(0.1948) - 1) * 100  #21.50679
```

    ## [1] 21.50679

``` r
# Asian Married
(exp(0.0881647) - 1) * 100  #9.216799
```

    ## [1] 9.216799

``` r
# Hispanic Married
(exp(0.1079) - 1) * 100  #11.39363
```

    ## [1] 11.39363

``` r
exp(0.161) - 1
```

    ## [1] 0.174685
