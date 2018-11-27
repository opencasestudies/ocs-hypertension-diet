# open-case-studies-Hypertension

This case study is part of the [OpenCaseStudies]() project. This work is licensed under the Creative Commons Attribution-NonCommercial 3.0 ([CC BY-NC 3.0](https://creativecommons.org/licenses/by-nc/3.0/us/)) United States License.


## Risk factors for hypertension using NYC NHANES data

The pathology of hypertension has been found with several medical studies, salt intaking is one of the most important risk factors for having hypertension. Instead of seeing the association of hypertension and salt intake, we focus on other factors that might have the impact on the odds of having hypertension.
The pathology of hypertension has been found with several medical studies, salt intaking is one of the most important risk factors for having hypertension. Instead of seeing the association of hypertension and salt intake, we focus on other factors that might have the impact on the odds of having hypertension.

Moreover, our case study also introduced the logistic regression and survey-weighted logistic regression, focusing on the difference of the two models and obviously survey-weighted logistic regression would be our choice of the model since we are using the survey data and it is weighted.




## Data


The National Health and Nutrition Examination Survey (NHANES) is a program of studies designed to assess the health and nutritional status of adults and children in the United States. The survey started in the 1970’s and became a continuous program since 1999. A large portion of the data is publicly available at https://wwwn.cdc.gov/nchs/nhanes/default.aspx. The R package nhanesA (https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html) may make it easier to explore the NHANES data for surveys conducted since 1999. Additionally, the linked mortality data are available for NHANES III and later surveys at this site: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm, which will be useful for exploring questions related to survival outcomes of the survey participants.

The data is downloaded from NYC NHANES (a local version of NHANES), The NYC Health and Nutrition Examination Survey (NYC HANES), modeled on the National Health and Nutrition Examination Survey, is a population-based, cross-sectional study with data collected from a physical examination and laboratory tests, as well as a face-to-face interview and an audio computer-assisted self-interview (ACASI).(cited from http://nychanes.org/data/)

## Analysis



Reducing the quality of diet from excellent to good increase the odds of having hypertension by 1.06 ($P < 0.001$), reducing the quality of diet from excellent to fair increase the odds of having hypertension by 1.13 ($P < 0.001$), reducing the quality of diet from excellent to fair reduce the odds of having hypertension by 0.98 ($P < 0.001$) and reducing the quality of diet from average of very good and good to poor increase the odds of having hypertension by 0.97  ($P < 0.001$).

The reduction of diet quality to levels below the current diet quality increase the odds of having hypertension. The excpetion was made by changing from excellent to poor. However, using the average of the very good and good to poor to replace that result, the reduction of the diet quality, on the average, leads to increase in the odds of hypertension substantially.  












