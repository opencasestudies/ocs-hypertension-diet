# osc-hypertension-diet

This case study is part of the [OpenCaseStudies](https://opencasestudies.github.io) project. This work is licensed under the Creative Commons Attribution-NonCommercial 3.0 ([CC BY-NC 3.0](https://creativecommons.org/licenses/by-nc/3.0/us/)) United States License.

The libraries used in this study are ` library(knitr)`, ` library(ggplot2)`, ` library(ggpubr)`, ` library(ggrepel)`,` library(tableone)`, ` library(kableExtra)`, ` library(survey)`, ` library(broom)`, `library(Rmisc)`, `library(tidyverse)`,`library(haven)`,`library(plotrix)`.


## Risk factors for hypertension using NYC NHANES data


The pathology of hypertension has been found with several medical studies, salt intaking is one of the most important risk factors for having hypertension. Instead of seeing the association of hypertension and salt intake, we focus on other factors that might have the impact on the odds of having hypertension.

Moreover, our case study also introduced the logistic regression and survey-weighted logistic regression, focusing on the difference of the two models and obviously survey-weighted logistic regression would be our choice of the model since we are using the survey data and it is weighted.




## Data


The National Health and Nutrition Examination Survey (NHANES) is a program of studies designed to assess the health and nutritional status of adults and children in the United States. The survey started in the 1970???s and became a continuous program since 1999. A large portion of the data is publicly available at https://wwwn.cdc.gov/nchs/nhanes/default.aspx. The R package nhanesA (https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html) may make it easier to explore the NHANES data for surveys conducted since 1999. Additionally, the linked mortality data are available for NHANES III and later surveys at this site: https://www.cdc.gov/nchs/data-linkage/mortality-public.htm, which will be useful for exploring questions related to survival outcomes of the survey participants.

The data is downloaded from NYC NHANES (a local version of NHANES), The NYC Health and Nutrition Examination Survey (NYC HANES), modeled on the National Health and Nutrition Examination Survey, is a population-based, cross-sectional study with data collected from a physical examination and laboratory tests, as well as a face-to-face interview and an audio computer-assisted self-interview (ACASI).(cited from http://nychanes.org/data/)

## Analysis


To select the most appropriate model, first, survey weight logistic regression (`svyglm`) is applied considering our survey weight data set. Next, we delete several insignificant variables based on their p-value. Then, we compare it with `glm`. In each step we begin with a simple model and then extend to the ideal one, a full model. This is well-grounded since we are able to analyze how the output changes by setting all other variables stable. Finally, we show the result table and visualize the comparison. 


## Alternative method

One package called `jtools` provides some functions to understand and sharethe results of (primarily) regression analyses efficiently. Besides the way we use in this case study, other functions in this package would be helpful to you if you want to have a quick way to see and compare the result. We would like to introduce some them and try to explore more if you are interested in them.

### `summ()`


The `summ()` function is an updated version of `summary()`, where 
you can set the 
[arguments](https://www.rdocumentation.org/packages/jtools/versions/2.0.0/topics/set_summ_defaults) 
to be used in every call to `summ()` 
in your script. Here are several options:

summ(model,exp = TRUE,model.info = FALSE, model.fit = FALSE)

* `model.info` : Toggles printing of basic information on sample size, name of DV, and number of predictors. If FALSE, these are not printed.
* `model.fit` : Toggles printing of model fit statistics. If FALSE, these are not printed.
* `exp` : If set it as TRUE, you will get the exponentiated coefficients rather than the linear estimates. 

This last option is pretty helpful for exponential family 
models, such as logistic regression.

### `export_summs()`

`export_summs()` in [package `huxtable`](https://cran.r-project.org/web/packages/huxtable/index.html) 
is designed to give 
you the features available in `summ()`. This is particularly
useful for putting the results of multiple models into a 
single table. Exponentiated coefficients, standard error 
(in the brackets) and other statistic parameters are provided.

### `plot_summs()`

The function `plot_summs()`, which is provided by [package 
`jtools`](https://cran.r-project.org/web/packages/jtools/index.html), 
plots regression coefficients and their associated
uncertainties (confidence intervals) in a visually appealing
way. One especially useful thing in our setting is that you 
can do the same procedure for multiple models simultaneously, 
and compare the results directly on one plot. We would like 
to introduce some basic options here:

* `...` : regression model(s)
* `coef` : If you would like to include only certain coefficients, provide them as a vector. If it is a named vector, then the names will be used in place of the variable names. Default: NULL
* `model.names` : If plotting multiple models simultaneously, you can provide a vector of names here. Default: NULL
* `ci_level`: The desired width of confidence intervals for the coefficients. Default: 0.95
* `scale`: If we set it as TRUE, it will make all variable in the same scale, making it pretty easy to make a quick assessment
* `coefs=`: The label names are default setting, but to make it more readable,
we can specify the label names via `coefs=`.



OpenCaseStudy Team: Stephanie Hicks, Leah Jager, Margaret Taub, Kexin Wang, Hanchao Zhang





