---
title: "Factors that contribute to hypertension"
author: "Kexin Wang, M.S."
output:
  html_document:
    keep_md: true
    highlight: tango
    number_sections: no
    theme: cosmo
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---



## Motivation

Sheena: Maybe add a little more information about hypertension: how prevalent is it, what is the public health cost of treatment, effect on life expectancy. Just to give some context on why understanding risk factors for hypertension is an important public health question. 

Hypertesion is one of the most common deseases in the world and it puts you at the risk of myocardial infarction, stroke, renal failure, and death if not detected early and treated appropriately. About 75 million American adults (32%) have high blood pressure—that’s 1 in every 3 adults. Besides, High blood pressure costs the nation $48.6 billion each year. This total includes the cost of health care services, medications to treat high blood pressure, and missed days of work.

The link between hypertension and some physical measurements has been well-established in previous studies. In this case study, moving beyond the traditional aspects, we will explore other risk factors and their potential influence on hypertension.

This case study also introduces logistic regression and survey-weighted logistic regression, focusing on the comparison between them. The result plot indicates that standard error of coefficients calculated by logistic regression is not accurate [Sheena: How does this plot indicate that the SEs are not accurate? I can see that they are different, but I can't tell by looking at the plot that one is better than the other. So maybe just say they are different, and we will discuss below which model is a better choice for this data set?]. So when we use survey data, survey-weight logistic regression is a good choice of model in this setting.

<center>
![](data/FinalPlot.png)

</center>

## What is the data?

Sheena: Please add some more information about NYC HANES: what was the main purpose of the survey, what was the recruitment/inclusion strategy, when were the data collected?

 [NYC HANES 2013-14 Blood Pressure Data](http://nychanes.org/data/): NYC HANES Analytics Datasets is downloaded here. Other useful resources can be found through it, such as 'Data Documents', 'Variable List','Analytics Guideline','Questionnaires'. All of them enable data users to understand the meaning and encoding of the variables better, and then complete the analysis more efficiently.
 
The NYC HANES is a population-based, cross-sectional study with data collected from a physical examination and laboratory tests, as well as a face-to-face interview and an audio computer-assisted self-interview (ACASI). From Augest 2013 to June 2014, the survey used a probability sample of non-institutionalized adult New York City residents (ages 20 years or older) to provide representative citywide estimates. For further details, please refer to its website.


### Other Resource


1. [National Health and Nutrition Examination Survey (NHANES)](https://wwwn.cdc.gov/nchs/nhanes/default.aspx): A program of studies designed to assess the health and nutritional status of adults and children in the United States.
NYC HANES is a local version of NHANES, which implies it mainly focus on New York area. Modeling on NHANES, NYC HANES collected data from a physical examination and laboratory tests, as well as interviews. 
2. [R package nhanesA](https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html): This package was developed to enable fully customizable retrieval of data from NHANES, such as load data, list variables in NHANES table. 
We may not use the package since we don't need to use data from NHANES. If you want to design a research based on NHANES data, this package would be definitiely helpful.

Sheena: You introduce this nhanesA package here, but do you actually use it at all? Maybe clarify when you would want to use this package and when you want to do as you do.

## Data import

### Load packages
Since this is the first R code chunk, we will load the necessary libraries.

```r
library(knitr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(tableone)
library(kableExtra)
library(survey)
library(broom)
library(Rmisc)
library(tidyverse)
library(haven)
library(jtools)
library(plotrix)
```


### Load data

The NYC HANES data file we are working with is a SAS formatted file so we would like to use the function `read_sas` from the `haven` library to read it into a data frame in R. The `haven` library is useful to import and export 'SAS', 'STATA', and 'SPSS' files.


```r
dat <- read_sas('./data/dat.sas7bdat')
```


This data frame contains 1527 observations of 704 different variables. In our analysis, only a subset of the variables will be selected.

## Data wrangling

### Select the variables that we are interested in

As we mentioned above, this is a survey data set based on interviews or questionnaires with 704 variables. Some variables are not relevant to our current research question, such as 'LAQ1: What language is being used to conduct this interview'. 

Previous people showed hypertension has relationship with drink, smoking, cholesterol values, and triglyceride levels. Excluding these, we will choose other covariates which might not at first seem to be be highly related to hypertension, such as income, and try to see whether they have a potential association with hypertension after all. Based on our interest, we selected 13 covariates for analysis.  


Here, we use the `select` function to choose and rename the columns that we want.


```r
hypertension_DF <- dat %>% 
  select(
id = KEY,
age = SPAGE,
race = DMQ_14_1,
gender = GENDER,
diet = DBQ_1,
income = INC20K,
diabetes = DX_DBTS,
bmi = BMI,
cholesterol = BPQ_16,
drink = ALQ_1_UNIT,
smoking = SMOKER3CAT,
hypertension = BPQ_2,
surveyweight = CAPI_WT
)
```
Here are explanations for these variables, from the codebook  http://nychanes.org/wp-content/uploads/sites/6/2019/01/28283961_NYC-HANES_codebook_Public_V3_011019.pdf :

Sheena: Can you include a path to the relevant codebook where these definitions came from?

-Non Categorical

  * id: Sample case ID
  * age: Sample age, range 22-115
  * bmi: $BMI = kg/m^2$ where kg is a person's weight in kilograms and $m^2$ is their height in meters squared
  * surveyweight: numeric values associated with each observation to let us know how much weight the observation should receive in our analysis. This variable will be discussed more later.
  
-Categorical

  * race: 
    + 100 = White
    + 110 = Black/African American
    + 120 = Indian
    + 140 = Native Hawaiian/Other Pacific Islander
    + 180 = Asian
    + 250 = Other race
  * gender:
    + 1 = Male
    + 2 = Female
  * born:
    + 1 = Us born
    + 2 = Other country
  * diet: 
    + 1 = Excellent
    + 2 = Very good 
    + 3 = Good
    + 4 = Fair
    + 5 = Poor
  * diabetes: Previously diagnosed with diabetes
    + 1 = Diabetic with diagnosis
    + 2 = Diabetic without diagnosis 
    + 3 = Not diabetic
  * cholesterol: An oil-based substance. If concentrations get too high, it puts people at risk of heart diseases
    + 1 = High cholesterol value
    + 2 = Low cholesterol value
  * drink: In the past 12 months, how often did sample drink any type of alcoholic beverage
    + 1 = Weekly
    + 2 = Monthly
    + 3 = Yearly
  * smoke: 
    + 1 = Never smoker
    + 2 = Current smoker
    + 3 = Former smoker
  * income:
    + 1 = Less than $20,000
    + 2 = $20,000 - $39,999
    + 3 = $40,000 - $59,999
    + 4 = $60,000 - $79,999
    + 5 = $80,000 - $99,999
    + 6 = $100,000 or more
  * hypertension: Previously diagnosed as having hypertension
    + 1 = Yes
    + 2 = No
    
### Initial data inspection

The first step of any data analysis should be to explore the data through data visualizations including tables and summary statistics. There are several ways that you can have a glance at your data. Plotting the data or using the `summary` or `head` functions are excellent ways to help you have a quick judgement on the data set.

The `summary` function tabulates categorical variables and provides summary statistics for continuous ones, while also including a count of missing values, which can be very important in deciding what variables to consider in downstream analysis.


```r
summary(hypertension_DF)
```

```
##       id                 age             race           gender    
##  Length:1527        Min.   :20.00   Min.   :100.0   Min.   :1.00  
##  Class :character   1st Qu.:30.00   1st Qu.:100.0   1st Qu.:1.00  
##  Mode  :character   Median :42.00   Median :110.0   Median :2.00  
##                     Mean   :44.55   Mean   :136.8   Mean   :1.58  
##                     3rd Qu.:57.00   3rd Qu.:180.0   3rd Qu.:2.00  
##                     Max.   :97.00   Max.   :250.0   Max.   :2.00  
##                                     NA's   :59                    
##       diet          income         diabetes          bmi       
##  Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :12.31  
##  1st Qu.:2.00   1st Qu.:1.000   1st Qu.:3.000   1st Qu.:23.33  
##  Median :3.00   Median :2.000   Median :3.000   Median :26.52  
##  Mean   :2.92   Mean   :2.985   Mean   :2.719   Mean   :27.73  
##  3rd Qu.:4.00   3rd Qu.:5.000   3rd Qu.:3.000   3rd Qu.:30.71  
##  Max.   :5.00   Max.   :6.000   Max.   :3.000   Max.   :69.17  
##  NA's   :3      NA's   :161     NA's   :281     NA's   :38     
##   cholesterol        drink          smoking       hypertension  
##  Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
##  1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000  
##  Median :2.000   Median :1.000   Median :1.000   Median :2.000  
##  Mean   :1.719   Mean   :1.726   Mean   :1.598   Mean   :1.726  
##  3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:2.000  
##  Max.   :2.000   Max.   :3.000   Max.   :3.000   Max.   :2.000  
##  NA's   :15      NA's   :412     NA's   :3       NA's   :3      
##   surveyweight  
##  Min.   : 1030  
##  1st Qu.: 2875  
##  Median : 3648  
##  Mean   : 4116  
##  3rd Qu.: 4989  
##  Max.   :15422  
## 
```
We find that certain variables have a large number of NA's; in particular 'drink' has 412 and 'diabetes' has 281. Directly removing rows containing missing data is not desirable considering the large number of such rows, so we decided to look more closely at the missing values. Checking the variable list again, we find another variable 'AlQ_1' : how often did the survey participant drink any type of alcoholic beverage? 0 means they never drink.

Let's see its distribution with function `table()`.


```r
table(dat$ALQ_1)
```

```
## 
##   0   1   2   3   4   5   6   7   8  10  12  14  15  16  17  20  24  25 
## 406 267 276 183 100  80  40  68   7  25   9   6   6   1   1  14   6   2 
##  28  30  40  50  60 100 144 180 189 200 365 
##   1   8   1   1   1   1   1   1   1   1   7
```
The result shows why there are so many missing values for 'drink': Among these 412 missing values, 406 samples never drink and there are just 6 real missing values. Therefore, merging these two variables as one is a better choice to avoid losing too many observations.


```r
hypertension_DF$drink[which(dat$ALQ_1==0)] <- 4
summary(hypertension_DF$drink)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   1.000   2.000   2.333   4.000   4.000       6
```
With the help of function `which()`, only 6 missing values for 'drink' are left now.

Sheena: what about the diabetes variable? Any idea why there are so many missing values there? Could it be people who don't know if they have diabetes, or something else?

In next step, we aim to remove rows containing missing data with a nice function `drop_na()` in library `tidyr` and store in a new dataframe:


```r
DF <- hypertension_DF %>%
  drop_na()
```

This will drop all rows which still contain missing values. Finally we retain 1065 observations of 13 different variables.

### Adjust data types

From the data summaries above, we can see that there are several categorical variables like race, gender, born, diet, income, diabetes, BMI, drink, and smoke, which are currently being treated as numerical values.

For these variables, it is important for us to adjust the coding, prior to doing any analysis in R, since different numeric values represent nothing but different categories. Sometimes people forget to change the type of data, and usually, it will give you a totally wrong result.

For example, ` lm()` and ` glm()` will treat numerically-coded categorical variables as continuous variables, which will give the wrong result. Instead, we want to convert these categorical variables to factors. Before doing this, we want to get a better understanding of exactly what values are stored in the different variables. We refer to the codebook to get the correct mapping of the numerical values to the category labels.

We can use the `factor` function to convert each variable and assign the correct levels. Any values that are not included in the 'levels' argument will get set to 'NA' values. We also want to think about creating a natural ordering to the factor levels here: the first level will generally be our reference level in a linear model, so it makes sense to try to give them an order that reflects this.

```r
DF$race <- factor(DF$race, levels=c(100, 110, 120, 140, 180, 250), 
labels=c('White', 'Black/African American', 
'Indian /Alaska Native',
'Pacific Islander', 'Asian', 'Other Race'))

DF$gender <-  factor(DF$gender, levels=c(1,2), 
labels=c('Male', 'Female'))

DF$diet <-  factor(DF$diet, levels=c(5:1), 
labels=c('Poor', 'Fair', 'Good', 'Very good','Excellent'))

DF$income <-  factor(DF$income, levels=c(1:6), 
labels=c('Less than $20,000','$20,000 - $39,999',
'$40,000 - $59,999','$60,000 - $79,999',
'$80,000 - $99,999','$100,000 or more'))

DF$diabetes <-  factor(DF$diabetes, levels=c(3,1,2), 
labels=c('Not diabetic','Diabetic dx','Diabetic but no dx'))

DF$cholesterol <-  factor(DF$cholesterol, levels=c(2,1), 
labels=c('Low value','High value'))

DF$drink <-  factor(DF$drink, levels=c(4,1,2,3), 
labels=c('Never','Weekly', 'Monthly', 'Yearly'))

DF$smoking <-  factor(DF$smoking, levels=c(3:1), 
labels=c('Never smoker','Former smoker','Current smoker'))

DF$hypertension <-  factor(DF$hypertension, levels=c(2,1), 
labels=c('No','Yes'))
```

```r
summary(DF)
```

```
##       id                 age                            race    
##  Length:1065        Min.   :20.00   White                 :491  
##  Class :character   1st Qu.:30.00   Black/African American:258  
##  Mode  :character   Median :42.00   Indian /Alaska Native :  7  
##                     Mean   :44.54   Pacific Islander      :  5  
##                     3rd Qu.:57.00   Asian                 :143  
##                     Max.   :94.00   Other Race            :161  
##     gender           diet                   income   
##  Male  :460   Poor     : 72   Less than $20,000:309  
##  Female:605   Fair     :235   $20,000 - $39,999:246  
##               Good     :391   $40,000 - $59,999:124  
##               Very good:264   $60,000 - $79,999:110  
##               Excellent:103   $80,000 - $99,999: 75  
##                               $100,000 or more :201  
##                diabetes        bmi            cholesterol      drink    
##  Not diabetic      :911   Min.   :15.02   Low value :758   Never  :255  
##  Diabetic dx       :126   1st Qu.:23.40   High value:307   Weekly :424  
##  Diabetic but no dx: 28   Median :26.47                    Monthly:201  
##                           Mean   :27.76                    Yearly :185  
##                           3rd Qu.:30.47                                 
##                           Max.   :69.17                                 
##            smoking    hypertension  surveyweight  
##  Never smoker  :230   No :775      Min.   : 1030  
##  Former smoker :204   Yes:290      1st Qu.: 2864  
##  Current smoker:631                Median : 3559  
##                                    Mean   : 4028  
##                                    3rd Qu.: 4786  
##                                    Max.   :15422
```


## Exploratory data analysis

Some simple data visualizations will help you to take a first look at the data and provide much information about how the different variables are related to one another. Plots can indicate the trends or patterns in the distributions of variables you are interested in, and inspire you in terms of the the next steps in your data analysis. For our visualizations, we will mainly use the package `ggplot2`, a powerful tool for data visualization. A link for its cheat sheet is here: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf.

Plotting numerical data is something you may be familiar with. This time we are going to incorporate some of the categorical variables into the plots. Although going from raw numerical data to categorical data bins does give you less precision, it can make drawing conclusions from plots much easier. 

First, we try to plot one categorical variable 'hypertension', with one numerical variable 'age'.


```r
p1 <- ggplot(DF, aes(x = hypertension, y=age))+
  geom_boxplot()+ggtitle('Distribution of age by hypertension status')
p1
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

What about hypertension with gender? Now let's try three different ways to plot the categorical variable 'gender'.


```r
p2 <- ggplot(DF, aes(x = hypertension, y=gender))+ 
  geom_boxplot()+ggtitle('distribution of gender')
p3 <- ggplot(DF, aes(x = gender, fill = hypertension)) + 
  geom_bar()+ggtitle('distribution of hypertension')
p4 <- ggplot(DF, aes(x = gender, fill = hypertension)) + 
  geom_bar(position = "fill")+ggtitle('distribution of hypertension') + ylab('proportion')
ggarrange(p2,p3,p4,ncol=3,nrow=1)
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The left plot uses `geom_boxplot()` as we did with 'age', but it fails to show the relationship! Boxplots are not what we want for a categorical variable like gender, so we need another way, instead of just applying traditional plotting methods. 

The next two plots show alternative ways of looking at the relationship between hypertension and gender. This time it works! First, we switch the x-axis and y-axis, to show hypertension as a function of gender, rather than the other way around, as hypertension is our outcome variable of interest. The middle plot shows the count of individuals with and without hypertension for males and females and you can compare the two distributions. But to more clearly see the proportion of people with hypertension among males and females for each level, we use `position='fill'`. The y-axis in the right plot is proportion rather than count. From this visualization, it is clear that a higher proportion of males than females in our dataset have hypertension. 

Then we can apply similiar visualization methods on other variables and find if there exists a realationship between them:

```r
p5 <- ggplot(DF, aes(x = smoking, fill = drink)) + 
  geom_bar(position = "fill")+ggtitle('smoking and drink') + ylab('proportion')
p6 <- ggplot(DF, aes(x = cholesterol, fill = diabetes)) + 
  geom_bar(position = "fill")+ggtitle('cholesterol and diabetes')+ ylab('proportion')

ggarrange(p5,p6,ncol=2,nrow=1)
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
To my mind, these plots are pretty illuminating. From the left plot, we see that there are more monthly and yearly drinkers in current smokers, and more weekly drinkers in former smokers. This may imply a potential relationship between smoking and drink. Additionally, the right plot shows that the proportion of diabetes patients are perfectly positive with high cholesterol value. Therefore potential relationship also exists in these two variables. 

What if we want to see relationship for more variables? This time we are going to incorporate three variables into the plots. 


```r
p7 <- ggplot(DF, aes(x = cholesterol, y = bmi, fill = cholesterol)) +
  geom_boxplot() +facet_wrap(~ smoking, ncol = 3)
p7
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

`facet_wrap(~ smoking, ncol = 3)` uses the faceting capability to produce a plot with three panels: one panel for each smoking category. Within each panel is a boxplot which characterises the distribution of bmi for each cholesterol catergory within that smoking category. We realize that there is a tendency of increasing bmi from never smoker to current smoker, and the highest bmi values are found in current smokers. This provides a view on the data which is complimentary to the previous boxplot above.

Based on these data visualizations, potential relationship is revealed. So we can choose which varibles we might want to include in our formal statistical modeling. 

Sheena: Can you include some visualizations of additional variables? You would want to do something similar for each variable of interest.



## Data analysis

Now that we have spent some time cleaning and looking at visualizations of our data, we can consider using a statistical model to address our question of interest about which of the factors we are considering are related to risk of hypertension.

What model can we use in our case? Since we are looking at whether or not someone develops hypertension, our outcome variable is **binary**. So you might think of logistic regression, and yes, you would be pretty close already. However, think of the nature of our dataset and how it was collected. It is data obtained from a survey, and there is an important point to consider for analysis of survey data.

It is possible, and indeed often happens even with a perfectly designed sampling plan, to end up with too many samples in a category. For example, too many women and not enough men, or too many whites and not enough other races or both. Data weighting makes sense for this kind of data. If we want the data to reflect the whole population, instead of treating each data point equally, we weight the data so that taken together, our sample can more accurately reflect the entire community.



### How are survey weights determined?


Suppose that you have 25 students (20 male and 5 female) in your class, and you want to talk with 5 of them to gauge their understanding of the biostatistics class. By sampling 5 students from the total 25 students, you might get 5 all female students or 4 female and 1 male in your sample. Do you expect this sample to represent the population? Definitely not since there is a higher proportion of females in the sample than the population. We can correct for this by weighting our samples so that, taken together, they better reflect the composition of the population we are generalizing to. 

The way of calculating the weight is:


$$Weight = \frac{Proportion~in~population}{Proportion~in~sample}$$
$$w_m=Male~Weight = \frac{20/25}{1/25} = 20$$
$$w_f=Female~Weight = \frac{5/25}{4/25} = 1.25$$

<center>
![](data/surveyweight.jpeg)
</center>

By weighting the observations, we can make the sample better represent the population.

When we have multiple strata on the data, it might be troublesome to calculate the weight. However, for many survey data sets such as ours, the appropriate weight has already been calculated and is included as a variable in the dataset. In our case study, the weight is calculated and we can simply apply this weight and perform a **survey-weighted logistic regression**.

Sheena: For some reason, the text above is centered; please fix.

### The weights we use

Because the NYC HANES 2013-2014 data have been collected to address a variety of different questions and using different surveys, the researchers who produced the data have employed a somewhat complex weighting scheme to compensate for unequal probability of selection. Five sets of survey weights have been constructed to correspond to different sets of variables that were collected: CAPI  weight, Physical weight, Blood Lab result weight, Urine Lab results weight and Salica Lab results weight. The determination of the most appropriate weight to use for a specific analysis depends upon the variables selected by the data analyst. When an analysis involves variables from different components of the survey, the analyst should decide whether the outcome is inclusive or exclusive, and then choose certain weights [Sheena: Can you clarify what you mean by 'inclusive' vs 'exclusive'? I'm not sure what these terms mean in this context.]. The website (http://nychanes.org/data/) provides guidelines about how to use weights for different purposes, and you can find them through the 'Analytic Guideline' and 'Other Training Materials' links. 

As the weight is given in the original dataset, we can use the variable 'surveyweight' directly in our analysis. Here we choose CAPI weight, which should be used to analyze participants responses to all interview questions. All 1527 survey participants have a value for the 'CAPI_WT' variable. We define hypertension as the prior diagnosis of high blood pressure, so we should use the most inclusive one, CAPI weight, to get an inclusive outcome. 

### What is the finite population correction factor?

Now that we have discussed how to weight our samples, there is one more technical detail that we need to address when using survey data. Many methods for analysis of survey data make the assumption that samples were collected using sampling with replacement, i.e., any time a sample is drawn, each person in the population has an equal chance of being sampled, even if they have already been sampled. This is not usually how surveys are actually carried out, so an adjustment may be necessary to reflect this difference. This adjustment is called the **finite population correction factor** and it is defined as:

$$FPC = (\frac{N-n}{N-1})^{\frac{1}{2}}$$
 
 + N = population size
 + n = sample size


The finite population correction (FPC) is used to reduce the variance when a substantial fraction of the total population of interest has been sampled. If you refer to 'Analytics Guideline' on NYC HANES website,  you can get the value of N and n. Next let's calculate the FPC as below:


```r
N <-  6825749
n <- 1065
((N-n)/(N-1))^0.5
```

```
## [1] 0.9999221
```

Sheena: Where did the value for N come from?

The FPC of our data set is very close to 1 and in general, you can ingore it. But technically, since the data were collected through sampling without replacement, it is more appropriate to use it.

### Create the survey weighted data

We now need to figure out how to incorporate our sampling weights in our modeling steps. To help us do this, there is a function ` svydesign()` in the R package ` survey`. This function combines a data frame and all the design information needed to specify a survey design. Here is the list of options provided in this function:

 + `ids`: Specify it for cluster sampling, ~0 or ~1 is a formula for no clusters. Cluster sampling is a multi-stage sampling, the total population are divided into several clusters and a simple random sample of clusters are selected. Each element in these clusters are then sampled.

 + `data`: Data frame to look up variables in the formula arguments, or database table name

 + `weights`: Formula or vector specifying sampling weights as an alternative to `prob`

 + `fpc`: Finite population correction, `~rep(N,n)`  generates a vector of length n where each entry is N (the population size). Default value is 1. The use of fpc derives a without replacement sample, otherwise the default is a with replacement sample.
 
 + `strata`: Specify it for stratified sampling, which divides members of the population into homogeneous subgroups and then sample independently in these subpopulations. It is advantageous when subpopulations within an overall population vary.
 
 
Now we use some options to create the design relative to our dataset:


```r
hypertension_design <- svydesign(
  id = ~1,
  #fpc = ~rep(N,n),
   weights = ~DF$surveyweight,
  data = DF[,-c(1,13)]
)
```
The arguments are interpreted as the following:

+ `ids = ~1` means there is no clustering.

+ `data = DF[,-c(1,13)]` tells `svydesign` where to find the actual data.

+ `weights= ~DF$surveyweight` tells it where to find the weight.

`summary()` shows the results:

```r
summary(hypertension_design)
```

```
## Independent Sampling design (with replacement)
## svydesign(id = ~1, weights = ~DF$surveyweight, data = DF[, -c(1, 
##     13)])
## Probabilities:
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 6.484e-05 2.089e-04 2.810e-04 2.921e-04 3.492e-04 9.705e-04 
## Data variables:
##  [1] "age"          "race"         "gender"       "diet"        
##  [5] "income"       "diabetes"     "bmi"          "cholesterol" 
##  [9] "drink"        "smoking"      "hypertension"
```

"Independent sampling design" means our sampling design is a simple random sample (SRS). When the population size is specified (via the fpc argument) it is assumed that the sampling is done without replacement. By setting other parameters you can also specify different kinds of designs, such as stratified or clustered sampling, etc.



### Survey-weighted summary statistics

Once we have created our `survey.design` object, we can do further analysis. It is very convenient to use the `svy*` functions which account for survey design features.

To calculate the mean and its standard error, use function `svymean()`. `svymean()` perform weighted estimation, with each observation being weighted rather than direct calculation. We would like to compare the result of traditional way with the help of `mean()` and `std.error()`:


```r
svymean(~bmi, hypertension_design)
```

```
##       mean     SE
## bmi 27.897 0.2188
```

```r
mean(DF$bmi)
```

```
## [1] 27.7636
```

```r
std.error(DF$bmi)
```

```
## [1] 0.2010068
```
It seems that there is not a very large difference between these two values and their standard errors. Since this is a simple random sample the difference is not that obvious. But ait will give precision estimates that incorporate the effects of stratification and clustering if you choose more complex sample design. [Sheena: why is this? I'm not quite clear how the mean is calcuated to take the survey design into account, and I think some further explanation here would be helpful.] 

To calculate the confidence interval, use function `confint()` directly:

```r
confint(svymean(~bmi, hypertension_design))
```

```
##        2.5 %   97.5 %
## bmi 27.46845 28.32625
```
Subgroup statistics is also easy to calculate with function `svyby()`:

```r
svyby(~bmi, by=~diet, design=hypertension_design, FUN = svymean)
```

```
##                diet      bmi        se
## Poor           Poor 29.50139 1.0511178
## Fair           Fair 30.30994 0.6104274
## Good           Good 27.51282 0.3045348
## Very good Very good 26.51232 0.3048522
## Excellent Excellent 26.04319 0.6233014
```
If you are particularly interested in one group, you can use function `subset()` :

```r
svymean(~bmi, subset(hypertension_design,gender=="Female"))
```

```
##       mean     SE
## bmi 28.075 0.3284
```

```r
svymean(~bmi, subset(hypertension_design,gender=="Male"))
```

```
##       mean     SE
## bmi 27.703 0.2843
```
It seems that mean bmi and the associated SEs are a little higher for females compared to males. 



### Fit a simple survey-weighted logistic regression

Logistic regression is widely used to deal with binary response variable. As we mentioned above, our dataset is a survey dataset, and we need to take survey weights into account, so therefore we need to fit the model with the survey weighted data. To do this, we can just use the `svyglm()` function from the `survey` package.

It's similar to fitting a normal logistic regression, and the only difference is that instead of using the original data set in the `data` argument, you should input the object from the output of `svydesign()` to the `design` argument in the function.

First, we need to recode our factor output as a 0-1 variable. [Sheena: Is this really necessary? Usually glm will just deal with two-level factors without recoding them.]


```r
hypertension_design$variables$hypertension <- 
  ifelse(hypertension_design$variables$hypertension == 'No', yes = 0, no =1)
```

Then we fit our survey-weighted logistic regression model and look at the summarized output. Firstly, just start with a simple model by choosing one variable. 'smoking' is chosen for test:


```r
g <- svyglm(hypertension ~ smoking, 
    family = quasibinomial(), design = hypertension_design)
summ(g,exp = TRUE)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Observations </td>
   <td style="text-align:right;"> 1065 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Dependent variable </td>
   <td style="text-align:right;"> hypertension </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Type </td>
   <td style="text-align:right;"> Survey-weighted generalized linear model </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Family </td>
   <td style="text-align:right;"> quasibinomial </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Link </td>
   <td style="text-align:right;"> logit </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Pseudo-R² (Cragg-Uhler) </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> Pseudo-R² (McFadden) </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> AIC </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table> <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> S.E. </th>
   <th style="text-align:right;"> t val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> -4.03 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smokingFormer smoker </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> -2.48 </td>
   <td style="text-align:right;"> 0.01 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smokingCurrent smoker </td>
   <td style="text-align:right;"> 0.69 </td>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> -2.04 </td>
   <td style="text-align:right;"> 0.04 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> Standard errors: Robust</td></tr></tfoot>
</table>
`summ()` is an updated version of `summary()`, you can set the arguments to be used in every call to `summ()` in your script/session. Find more informatin via https://www.rdocumentation.org/packages/jtools/versions/2.0.0/topics/set_summ_defaults. Here we would give several options:

+ `model.info` : Toggles printing of basic information on sample size, name of DV, and number of predictors. If FALSE, these are not printed.

+ `model.fit` : Toggles printing of model fit statistics. If FALSE, these are not printed.

+ `exp` : If set it as TRUE, you will get the exponentiated coefficients rather than the linear estimates. This option is pretty helpful for exponential family models, such as logistic regression.

What does the model output tells us about the relationship between predictor variables and the chance of getting hypertension? Look at coefficient table. For predictor variable 'smoking', baseline setting is 'Never smoker'. Exponentiated coefficients for 'Former smoker' is 0.56, which means 'Former smoker' is associated with an odd of getting hypertension of 56% of that for 'Never smoker'; Similiarly, 'Current smoker' is associated with an odd of getting hypertension of 69% of that for 'Never smoker'. What's more, we can see the p-value for 'smoking' are all less than 0.05, suggesting their significance in this case.

### Fit a full model

After being familiar with how to interpret output of logistic regression model, try to fit a full model including all variables: 


```r
g1 <- svyglm(hypertension ~ 
    age+race+gender+diet+income+diabetes+bmi+cholesterol+drink+smoking, 
    family = quasibinomial(), design = hypertension_design)
summ(g1,exp = TRUE, model.info = FALSE, model.fit = FALSE)
```

  <table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> exp(Est.) </th>
   <th style="text-align:right;"> S.E. </th>
   <th style="text-align:right;"> t val. </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;font-weight: bold;"> (Intercept) </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:right;"> -6.56 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> age </td>
   <td style="text-align:right;"> 1.04 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 5.65 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> raceBlack/African American </td>
   <td style="text-align:right;"> 2.05 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 3.03 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> raceIndian /Alaska Native </td>
   <td style="text-align:right;"> 0.23 </td>
   <td style="text-align:right;"> 1.07 </td>
   <td style="text-align:right;"> -1.40 </td>
   <td style="text-align:right;"> 0.16 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> racePacific Islander </td>
   <td style="text-align:right;"> 11.66 </td>
   <td style="text-align:right;"> 1.38 </td>
   <td style="text-align:right;"> 1.78 </td>
   <td style="text-align:right;"> 0.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> raceAsian </td>
   <td style="text-align:right;"> 1.31 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> 0.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> raceOther Race </td>
   <td style="text-align:right;"> 1.19 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:right;"> 0.58 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> genderFemale </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> -2.27 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> dietFair </td>
   <td style="text-align:right;"> 1.24 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> dietGood </td>
   <td style="text-align:right;"> 1.24 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.63 </td>
   <td style="text-align:right;"> 0.53 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> dietVery good </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:right;"> 0.37 </td>
   <td style="text-align:right;"> -0.19 </td>
   <td style="text-align:right;"> 0.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> dietExcellent </td>
   <td style="text-align:right;"> 0.96 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> -0.10 </td>
   <td style="text-align:right;"> 0.92 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> income$20,000 - $39,999 </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> -2.36 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> income$40,000 - $59,999 </td>
   <td style="text-align:right;"> 0.43 </td>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> -2.42 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> income$60,000 - $79,999 </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> -1.03 </td>
   <td style="text-align:right;"> 0.30 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> income$80,000 - $99,999 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.53 </td>
   <td style="text-align:right;"> -1.47 </td>
   <td style="text-align:right;"> 0.14 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> income$100,000 or more </td>
   <td style="text-align:right;"> 0.49 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> -2.40 </td>
   <td style="text-align:right;"> 0.02 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> diabetesDiabetic dx </td>
   <td style="text-align:right;"> 2.56 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 3.59 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> diabetesDiabetic but no dx </td>
   <td style="text-align:right;"> 1.53 </td>
   <td style="text-align:right;"> 0.43 </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:right;"> 0.32 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> bmi </td>
   <td style="text-align:right;"> 1.07 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 4.86 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> cholesterolHigh value </td>
   <td style="text-align:right;"> 2.36 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 4.52 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> drinkWeekly </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> -0.07 </td>
   <td style="text-align:right;"> 0.95 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> drinkMonthly </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> -0.97 </td>
   <td style="text-align:right;"> 0.33 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> drinkYearly </td>
   <td style="text-align:right;"> 1.08 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> 0.77 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smokingFormer smoker </td>
   <td style="text-align:right;"> 0.70 </td>
   <td style="text-align:right;"> 0.29 </td>
   <td style="text-align:right;"> -1.22 </td>
   <td style="text-align:right;"> 0.22 </td>
  </tr>
  <tr>
   <td style="text-align:left;font-weight: bold;"> smokingCurrent smoker </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> -0.40 </td>
   <td style="text-align:right;"> 0.69 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> Standard errors: Robust</td></tr></tfoot>
</table>

Here we just keep the coefficient table. 

One interesting thing is that both the exponentiated coefficients and the p-value for 'smoking' in this model increase sharply! Why will we get this change? Do you remember in section 'Exploary data analysis', we find there is a correlation between 'smoking' and other varables. This finding proves that when adding other variables, influence of former predictor variables are likely to change.

Now, let's interpret the new output. We would like to choose one catergorical variable and one continuous variable:

+ smoking: Holding all other variables the same, 'Former smoker' is associated with an odd of getting hypertension of 70% of that for 'Never smoker'; 'Current smoker' is associated with an odd of getting hypertension of 91% of that for 'Never smoker'. 

+ bmi: Holding all other variables the same, a unit increase in bmi changes odds by 1.07.

This step is pretty meaningful since it provides us more accurate result. When we try to understand the output, holding all other variables the same is crucial. If we just do analysis based on the simple model, we can not guarantee all other variables keep constant. However, the advantage of full model is that we compare all variables with others stable, maximizing the chance to perform the same level comparison. Therefore our interpretation could be more precise and rational.

Sheena: The summ(g1) object has issues rendering in markdown; I am not sure why. But when I uncomment `summ(g1)` the markdown file no longer renders. Can you play around with this and figure out how to fix it so that a table of the model results can be displayed in the knitted document? Once you have done that, please add a paragraph here describing and interpreting the results that you find from your model. What does the model output tell you about the relationship between your predictor variables and the chance of getting hypertension? I would actually also suggest starting with a more simple model, wiht just one predictor, and write a summary paragraph describing what the model results tell you. This would give a good review of interpreting output of logistic regression models.



### Model selection

Not all variables in g1 are significant so we would like to remove some of them to get a reduced model. We find 'race','income', 'diet','drink' and 'smoke' are not that sigifincant. Contrary to our common sense, 'diet','drink' and 'smoke' have minimum influence on hypertension when including the other variables in the model. We believe this is because that their influence may be explained by other covariates, such as 'bmi'. Therefore, here we just remove 'race' and 'income'.


```r
g2 <- svyglm(hypertension ~ 
        age+bmi+cholesterol+diabetes+gender+smoking+diet, 
        family=quasibinomial(), design = hypertension_design)
```

[Sheena: I am getting a warning when I run this: "Warning message:
In eval(family$initialize) : non-integer #successes in a binomial glm!" Do you know why?]

Using this model output, we would like to further examine which covariates have the larger influence on hypertension. To do this, we compare their effect sizes in the plot below.


```r
s <- data.frame('c'=exp(g2$coefficients[2:12]),
                'n'=names(g2$coefficients)[2:12])
s <- s[order(s$c),]
l=seq(from=2, to=12, by=1)

plt<- ggplot(s,aes(x = l, y=c,col=s$c)) + 
  geom_point(size=5)+ 
  scale_colour_gradient(low ='pink', high = 'darkred')+
  geom_hline(yintercept = 1,col='pink',size=1.5,linetype=2)+
  theme_bw() + theme(plot.title = element_text(hjust = 2,vjust=-2))+
  geom_text_repel(aes(y = c,label=n),col='darkred')+
  ggtitle("Effect on Hypertension") + 
  ylab("Feature Effects") + 
  xlab("Features")+
  coord_flip()
  
plt
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

Here we plot the exponentiated coefficients, so that the effect is an odds ratio. If the value is larger than 1, the value of the corvariate shown is associated with an increased probability of getting hypertension, and vice versa. Moreover, a larger absolute value indicates a larger influence on response variable. 

We can conclude that people with diabetes, high value of cholesterol and fair diet have higher risk of getting hypertension compared to individuals with the baseline levels of these covariates. Also, among them, diabetes and cholesterol have the strongest relationship with hypertension, which is an accurate reflection of what we already know about hypertension. In all, with the help of a regression model like this, researchers could make an initial judgement about whether an individual may have a high risk of hypertension, once these variables have been measured.

### Compare `glm()` and `svyglm()`

What would happen in this case if we were to use standard logistic regression? 


```r
g_svy <- glm(hypertension~ smoking, 
          family = binomial(link = 'logit'),data = DF)
export_summs(g, g_svy,exp = TRUE, model.names = c('glm','svyglm'))
```

<!--html_preserve--><table class="huxtable" style="border-collapse: collapse; margin-bottom: 2em; margin-top: 2em; width: 50%; margin-left: auto; margin-right: auto;  ">
<col><col><col><tr>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0pt 0pt; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">glm</td>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">svyglm</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(Intercept)</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.54 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.48 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.15)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.14)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">smokingFormer smoker</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.56 *&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.67&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.23)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.22)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">smokingCurrent smoker</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.69 *&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.73&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">(0.18)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">(0.17)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">N</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1065&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1065&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">R2</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">AIC</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1259.09&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1248.80&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">BIC</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1263.71&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">Pseudo R2</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">0.00&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">0.01&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td colspan="3" style="vertical-align: top; text-align: left; white-space: normal; padding: 4pt 4pt 4pt 4pt;"> *** p &lt; 0.001;  ** p &lt; 0.01;  * p &lt; 0.05.</td>
</tr>
</table>
<!--/html_preserve-->
`export_summs()` in package `huxtable` is designed to give you the features available in `summ()`. This is particularly useful for putting the results of multiple models into a single table. Exponentiated coefficients, standard error (in the brackets) and other statistic parametes are provided.

From the output, we see that all of the coefficients value rise. Therefore, compared with those who never smoke, the odds of getting hypertension for former smokers and current smokers also change to 0.67 and 0.73 respectively; Second, standard errors calculated by svyglm are less than that calculated by glm, leading to a more accurate confidence interval; In conclusion, svyglm does better job than glm.

In the next step, let's compare the full model designed by these two methods. 


```r
g3 <- glm(hypertension~ age+bmi+cholesterol+diabetes+gender+smoking+diet, 
          family = binomial(link = 'logit'),data = DF)
export_summs(g2, g3,exp = TRUE, model.names = c('glm','svyglm'))
```

<!--html_preserve--><table class="huxtable" style="border-collapse: collapse; margin-bottom: 2em; margin-top: 2em; width: 50%; margin-left: auto; margin-right: auto;  ">
<col><col><col><tr>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0pt 0pt; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">glm</td>
<td style="vertical-align: top; text-align: center; white-space: nowrap; border-style: solid solid solid solid; border-width: 0.8pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">svyglm</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(Intercept)</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.01 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.00 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.65)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.58)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">age</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.04 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.04 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.01)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.01)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">bmi</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.07 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.08 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.01)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.01)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">cholesterolHigh value</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.94 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.79 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.18)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.17)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">diabetesDiabetic dx</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">3.15 ***</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">2.71 ***</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.24)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.22)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">diabetesDiabetic but no dx</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.92&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.71&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.46)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.42)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">genderFemale</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.69 *&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.70 *&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.18)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.16)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">smokingFormer smoker</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.85&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.10&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.28)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.25)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">smokingCurrent smoker</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.02&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.09&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.23)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.20)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">dietFair</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.49&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.22&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.35)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.34)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">dietGood</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1.23&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.97&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.35)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.33)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">dietVery good</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.87&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.69&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.37)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">(0.35)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">dietExcellent</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.91&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">0.71&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;"></td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">(0.44)&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.4pt 0pt; padding: 4pt 4pt 4pt 4pt;">(0.41)&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">N</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1065&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1065&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">R2</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">AIC</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1031.15&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1007.31&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">BIC</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; padding: 4pt 4pt 4pt 4pt;">1071.93&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td style="vertical-align: top; text-align: left; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">Pseudo R2</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">0.07&nbsp;&nbsp;&nbsp;&nbsp;</td>
<td style="vertical-align: top; text-align: right; white-space: nowrap; border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt; padding: 4pt 4pt 4pt 4pt;">0.32&nbsp;&nbsp;&nbsp;&nbsp;</td>
</tr>
<tr>
<td colspan="3" style="vertical-align: top; text-align: left; white-space: normal; padding: 4pt 4pt 4pt 4pt;"> *** p &lt; 0.001;  ** p &lt; 0.01;  * p &lt; 0.05.</td>
</tr>
</table>
<!--/html_preserve-->
It is obvious 'diabetes' and 'diet' differ in exponentiated coefficients while that for the others vary slightly. Unlike the simple model, there are too many predict variables in full model and it is cumbersome to compare them one by one. To guarantee a straight perception, we would like to visualize model output rather than show them in a lengthy table. 

Sheena: Include a paragraph here describing the results from this model and making some initial comparisons with the survey-weighted results.

### Visualization of model output using `plot_summs`

The function `plot_summs()`, which is provided by library `jtools`, plots regression coefficients and their associated uncertainties (confidence intervals) in a visually appealing way. One especially useful thing in our setting is that you can do the same procedure for multiple models simultaneously, and compare the results directly on one plot. We would like to introduce some basic options here:

+ `...` : regression model(s)

+ `coef` : If you would like to include only certain coefficients, provide them as a vector. If it is a named vector, then the names will be used in place of the variable names. Default: NULL

+ `model.names` : If plotting multiple models simultaneously, you can provide a vector of names here. Default: NULL

+ `ci_level`: The desired width of confidence intervals for the coefficients. Default: 0.95

+ `scale`: If we set it as TRUE, it will make all variable in the same scale, making it pretty easy to make a quick assessment



```r
plot_summs(g2, g3,scale = TRUE, 
           model.names = c("svyglm", "glm"))
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

The label names are default setting, but to make it more readable, we can specify the label names via `coefs=`.


```r
plot_summs(g2,g3,
coefs = c('age'='age','bmi'='bmi', 'cholesterol'='cholesterol',
'diabetic with diagnosis'='diabetesDiabetic dx',
'diabetic without diagnosis'='diabetesDiabetic but no dx',
'gender'='gender','former smoker'='smokingFormer smoker',
'current smoker'='smokingCurrent smoker',
'fair diet'='dietFair','good diet'='dietGood',
'very good diet'='dietVery good','excellent diet'='dietExcellent'),
 model.names = c("svyglm", "glm"), 
scale=TRUE)+ xlab('confidence interval')
```

![](Hypertension_and_Diet_files/figure-html/unnamed-chunk-25-1.png)<!-- -->


```r
ggsave("data/Finalplot.png", plot = last_plot(), device = "png")
```
Obviously, 'cholesterol', 'diabetes', 'smoke' and 'drink' have larger differences in confidence intervals comparing the standard glm to the survey-weighted glm restuls, while 'age', 'bmi' and 'gender' have only small differences. 

Different with the result when we compare simple models, the confidence interval computed by `svyglm`(ci_svyglm) is larger than that computed by `glm` (ci_glm). It is strange, isn't it? One possible reason behind this is `survey` computes the standard errors with consideration of the loss of precision introduced by sampling weights. Weights in `glm` simply adjust the weight given to the errors in the least square estimation, so the standard errors aren't correct in our context. 


## Summary and Results

In this study, we mainly figure out the risk factors that contribute to hypertension and uses logistic regression to deal with binary response variable. Survery weight data is the most important topic in this study. Thus, we apply two approaches to analyze data and compare their results, one is general logistic regression and the other is survey weight logistic regression. 

To select the most appropriate model, in the first step, survey weight logistic regression (`svyglm`) is applied considering our survey weight data set. Next, we delete several insignificant variables based on their p-value. Then, we compare it with `glm`. In each step we begin with a simple model and then extend to the ideal one, a full model. This is well-grounded since we are able to analyze how the output changes by setting all other variables stable. There is no doubt the same condition is the cornerstone of  precise interpretation.

In output interpretation, we have taught you how to analyze the odds of getting hypertension with one categorical variable example and one numerical variable example. Besides, we find 'diabetes' and 'cholesterol' have strong positive effects on the probability of getting hypertension. And confidence intervals for 'cholesterol', 'diabetes' ,'smoke' and 'drink' have obvious difference. It is wired that in simple model, ci_svyglm is shorter than ci_glm while in full model the results change to the opposite. One potential explanation is the incompleteness of `glm`. But `svyglm` are more likely to give an accurate results since it consider sampling weight. Therefore we still believe, for survey weight data, `svyglm` should be the first choice than others.

In the future, we can make more efforts on applied survey methods and decide whether there exist better approaches, such as stratified sample. 

