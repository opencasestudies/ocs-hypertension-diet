# open-case-studies-Hypertension

Sheena: Update the package list to include all necessary packages

This case study is part of the [OpenCaseStudies]() project. This work is licensed under the Creative Commons Attribution-NonCommercial 3.0 ([CC BY-NC 3.0](https://creativecommons.org/licenses/by-nc/3.0/us/)) United States License.

The libraries used in this study are ` library(knitr)`, ` library(ggplot2)`, ` library(ggpubr)`, ` library(ggrepel)`,` library(tableone)`, ` library(kableExtra)`, ` library(survey)`, ` library(broom)`, `library(Rmisc)`, `library(tidyverse)`,`library(haven)`,`library(jtools)`,`library(plotrix)`.


## Risk factors for hypertension using NYC NHANES data


The pathology of hypertension has been found with several medical studies, salt intaking is one of the most important risk factors for having hypertension. Instead of seeing the association of hypertension and salt intake, we focus on other factors that might have the impact on the odds of having hypertension.

Moreover, our case study also introduced the logistic regression and survey-weighted logistic regression, focusing on the difference of the two models and obviously survey-weighted logistic regression would be our choice of the model since we are using the survey data and it is weighted.




## Data


The National Health and Nutrition Examination Survey (NHANES) is a program of studies designed to assess the health and nutritional status of adults and children in the United States. The survey started in the 1970???s and became a continuous program since 1999. A large portion of the data is publicly available at https://wwwn.cdc.gov/nchs/nhanes/default.aspx. The R package nhanesA (https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html) may make it easier to explore the NHANES data for surveys conducted since 1999. Additionally, the linked mortality data are available for NHANES III and later surveys at this site: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm, which will be useful for exploring questions related to survival outcomes of the survey participants.

The data is downloaded from NYC NHANES (a local version of NHANES), The NYC Health and Nutrition Examination Survey (NYC HANES), modeled on the National Health and Nutrition Examination Survey, is a population-based, cross-sectional study with data collected from a physical examination and laboratory tests, as well as a face-to-face interview and an audio computer-assisted self-interview (ACASI).(cited from http://nychanes.org/data/)

## Analysis


To select the most appropriate model, first, survey weight logistic regression (`svyglm`) is applied considering our survey weight data set. Next, we delete several insignificant variables based on their p-value. Then, we compare it with `glm`. In each step we begin with a simple model and then extend to the ideal one, a full model. This is well-grounded since we are able to analyze how the output changes by setting all other variables stable. Finally, we show the result table and visualize the comparison. 











