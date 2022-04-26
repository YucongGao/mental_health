final\_project
================
Yucong Gao
4/22/2022

### Package Loading

``` r
# Function: installing and loading of packages
install_load <- function (packages)  {   
   
  # Start loop to determine if each package is installed
   for(package in packages){

       # If package is installed locally, load
       if(package %in% rownames(installed.packages()))
          do.call('library', list(package))

       # If package is not installed locally, download, then load
       else {
          install.packages(package, dependencies = TRUE)
          do.call("library", list(package))
       }
   } 
}

# Generic libraries loading
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
install_load(libs)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:plotly':
    ## 
    ##     arrange, mutate, rename, summarise

    ## The following object is masked from 'package:maps':
    ## 
    ##     ozone

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## Loading required package: sp

    ## ### Welcome to rworldmap ###

    ## For a short introduction type :   vignette('rworldmap')

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     extract

    ## The following object is masked from 'package:reshape2':
    ## 
    ##     smiths

    ## Warning: package 'DT' was built under R version 4.1.2

``` r
# Specific methods libraries loading
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)
```

    ## Warning: package 'C50' was built under R version 4.1.2

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-3

    ## Warning: package 'randomForest' was built under R version 4.1.2

    ## randomForest 4.7-1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:psych':
    ## 
    ##     outlier

``` r
# Data loading
data <- read.csv("./survey.csv")
```

## Data Cleaning

``` r
# To delete no important elements
data <- data[ , !(names(data) %in% "state")]
data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
data <- data[ , !(names(data) %in% "self_employed")]


# Gender unification.
data$Gender %<>% str_to_lower()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% male_str) "male" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% female_str) "female" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% trans_str) "trans" else x )
data %<>% filter(Gender != "a little about you")
data %<>% filter(Gender != "guy (-ish) ^_^")
data %<>% filter(Gender != "p")


# Age categorization
data$Age<-cut(data$Age, c(-Inf,20,35,65,Inf))

# NA values detection and deleting the row.
sapply(data, function(x) sum(is.na(x)))
```

    ##                       Age                    Gender                   Country 
    ##                         0                         0                         0 
    ##            family_history                 treatment            work_interfere 
    ##                         0                         0                       264 
    ##              no_employees               remote_work              tech_company 
    ##                         0                         0                         0 
    ##                  benefits              care_options          wellness_program 
    ##                         0                         0                         0 
    ##                 seek_help                 anonymity                     leave 
    ##                         0                         0                         0 
    ## mental_health_consequence   phys_health_consequence                 coworkers 
    ##                         0                         0                         0 
    ##                supervisor   mental_health_interview     phys_health_interview 
    ##                         0                         0                         0 
    ##        mental_vs_physical           obs_consequence 
    ##                         0                         0

``` r
data <- data[!is.na(data$work_interfere),]

# Saving the original data with all importants variables 
data.origin <- data
```

### Convert data to categories

age: (-Inf,20\] \~ 1, (20,35\] \~ 2, (35,65\] \~ 3, (65, Inf\] \~ 4 <br>
gender: male \~ 1, female \~ 2, trans \~ 3 <br> country: United State \~
1, others \~ 0 <br> family\_history: yes \~ 1, no \~ 0 <br>
work\_inference: never \~ 1, rarely \~ 2, sometimes \~ 3, often \~ 4
<br> no\_employees: 1-5 \~ 1, 6-25 \~ 2, 26-100 \~ 3, 100-500 \~ 4,
500-1000 \~ 5, more than 1000 \~ 6 <br> remote\_work: yes \~ 1, no \~ 0
<br> tech\_company: yes \~ 1, no \~ 0 <br> *benefits: yes \~ 1, no \~ 2,
don’t know \~ 3 <br> *care\_options: yes \~ 1, no \~ 2, TRUE \~ 3 <br>
wellness\_program: yes \~ 1, no \~ 2, don’t know \~ 3 <br> seek\_help:
yes \~ 1, no \~ 2, don’t know \~ 3 <br> 2*anonymity: yes \~ 1, no \~ 2,
don’t know \~ 3 <br> 1*leave: Very easy \~ 1, Somewhat easy \~ 2,
Somewhat difficult \~ 3, Very difficult \~ 4, Don’t know \~ 5 <br>
mental\_health\_consequence: Yes \~ 1, No \~ 2, Maybe \~ 3 <br>
phys\_health\_consequence: Yes \~ 1, No \~ 2, Maybe \~ 3 <br> coworkers:
Yes \~ 1, No \~ 2, Some of them \~ 3 <br> supervisor: Yes \~ 1, No \~ 2,
Some of them \~ 3 <br> mental\_health\_interview: Yes \~ 1, No \~ 2,
Maybe \~ 3 <br> phys\_health\_interview: Yes \~ 1, No \~ 2, Maybe \~ 3
<br> 3\*mental\_vs\_physical: yes \~ 1, no \~ 2, don’t know \~ 3 <br>
obs\_consequence: Yes \~ 1, No \~ 0 <br>

``` r
dat = tibble(data.origin) %>% janitor::clean_names()
dat = 
   dat %>% 
   select(-leave, -anonymity, -mental_vs_physical, -care_options) %>% 
   filter(benefits != "Don't know") %>% 
   filter(wellness_program != "Don't know") %>% 
   filter(seek_help != "Don't know") 
   

dat_tibble = 
   dat %>% 
   mutate(age = factor(case_when(age == "(-Inf,20]" ~ 1, 
                                 age == "(20,35]" ~ 2, 
                                 age == "(35,65]" ~ 3, 
                                 age == "(65, Inf]" ~ 4)), 
          gender = factor(case_when(gender == "male" ~ 1, 
                                    gender == "female" ~ 2, 
                                    TRUE ~ 3)), 
          country = factor(case_when(country == "United States" ~ 1, 
                                     TRUE ~ 2)), 
          family_history = factor(case_when(family_history == "Yes" ~ 1, 
                                            TRUE ~ 2)), 
          work_interfere = factor(case_when(work_interfere == "Never" ~ 1, 
                                            work_interfere == "Rarely" ~ 2,
                                            work_interfere == "Sometimes" ~ 3,
                                            work_interfere == "Often" ~ 4)), 
          no_employees = factor(case_when(no_employees == "1-5" ~ 1, 
                                          no_employees == "6-25" ~ 2,
                                          no_employees == "26-100" ~ 3,
                                          no_employees == "100-500" ~ 4,
                                          no_employees == "500-1000" ~ 5,
                                          no_employees == "More than 1000" ~ 6)), 
          remote_work = factor(case_when(remote_work == "Yes" ~ 1, 
                                         TRUE ~ 2)), 
          tech_company = factor(case_when(tech_company == "Yes" ~ 1, 
                                         TRUE ~ 2)), 
          benefits = factor(case_when(benefits == "Yes" ~ 1, 
                                      benefits == "No" ~ 2,
                                      T ~ 3)), 
          wellness_program = factor(case_when(wellness_program == "Yes" ~ 1, 
                                              wellness_program == "No" ~ 2,
                                              TRUE ~ 3)), 
          seek_help = factor(case_when(seek_help == "Yes" ~ 1, 
                                       seek_help == "No" ~ 2,
                                       TRUE ~ 3)), 
          mental_health_consequence = factor(case_when(mental_health_consequence == "Yes" ~ 1, 
                                                       mental_health_consequence == "No" ~ 2,
                                                       TRUE ~ 3)), 
          phys_health_consequence = factor(case_when(phys_health_consequence == "Yes" ~ 1, 
                                                     phys_health_consequence == "No" ~ 2,
                                                     TRUE ~ 3)),
          coworkers = factor(case_when(coworkers == "Yes" ~ 1, 
                                       coworkers == "No" ~ 2,
                                       TRUE ~ 3)),
          supervisor = factor(case_when(supervisor == "Yes" ~ 1, 
                                       supervisor == "No" ~ 2,
                                       TRUE ~ 3)),
          mental_health_interview = factor(case_when(mental_health_interview == "Yes" ~ 1, 
                                       mental_health_interview == "No" ~ 2,
                                       TRUE ~ 3)),
          phys_health_interview = factor(case_when(phys_health_interview == "Yes" ~ 1, 
                                       phys_health_interview == "No" ~ 2,
                                       TRUE ~ 3)),
          obs_consequence = factor(case_when(obs_consequence == "Yes" ~ 1, 
                                             TRUE ~ 2)))





dat_df = as.data.frame(dat_tibble)
```

## PCA

``` r
features = dat_df[, c(6:19)]
for (i in 1:length(features)){
   features[,i] = as.numeric(features[,i])
}


features = features[,c(-2, -7)]
```

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
a = cor(features)
corrplot(a)
```

![](final_project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

demographic variables (age, gender, country, state, family history) are
not included in the PCA response variable - treatment also not included
in the pca

Parallel analysis finds that 4 eigenvalue larger than the expected
“random chance” eigenvalues based on randomly resampled data. So in the
following exploratory factor analysis, we decide to explore 3, 4, 5, 6
factors to fit the model

``` r
depressionprl = fa.parallel(features, cor = "poly", fa = "pc")
```

    ## Warning in polychoric(x, correct = correct): The items do not have an equal
    ## number of response alternatives, global set to FALSE.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 3 cells were adjusted
    ## for 0 values using the correction for continuity. Examine your data carefully.

![](final_project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  NA  and the number of components =  4

``` r
depressionprl$pc.values
```

    ##  [1] 2.2202747 1.6207404 1.4952292 1.3022143 1.0687520 0.9458471 0.7893807
    ##  [8] 0.7346855 0.6169542 0.5244556 0.4637639 0.2177023

``` r
depressionprl$pc.sim
```

    ##  [1] 1.2476688 1.1816451 1.1204212 1.0888611 1.0528249 1.0148732 0.9768757
    ##  [8] 0.9386681 0.9058747 0.8695791 0.8257238 0.7769842

## Exploratory Factor Analyais

``` r
# data to use for efa
dep_efa = features
res_3 = fa(dep_efa, 3, cor = "poly",  rotate = "geominQ", fm = "wls")
```

    ## Warning in polychoric(r, correct = correct, weight = weight): The items do not
    ## have an equal number of response alternatives, global set to FALSE.

    ## Warning in matpLower(x, nvar, gminx, gmaxx, gminy, gmaxy): 3 cells were adjusted
    ## for 0 values using the correction for continuity. Examine your data carefully.

``` r
res_3$loadings
```

    ## 
    ## Loadings:
    ##                           WLS1   WLS2   WLS3  
    ## work_interfere                          -0.239
    ## remote_work               -0.347        -0.222
    ## tech_company              -0.384        -0.277
    ## benefits                   0.774        -0.112
    ## wellness_program           0.932              
    ## mental_health_consequence -0.118  0.266  0.436
    ## phys_health_consequence           0.315  0.139
    ## coworkers                         0.586       
    ## supervisor                 0.182  0.585 -0.107
    ## mental_health_interview                  0.248
    ## phys_health_interview             0.141  0.141
    ## obs_consequence                  -0.149  0.576
    ## 
    ##                 WLS1  WLS2  WLS3
    ## SS loadings    1.806 0.910 0.832
    ## Proportion Var 0.150 0.076 0.069
    ## Cumulative Var 0.150 0.226 0.296

``` r
res_4 = fa(dep_efa, 4, cor = "poly", rotate = "geominQ", fm = "wls")
```

    ## Warning in polychoric(r, correct = correct, weight = weight): The items do not
    ## have an equal number of response alternatives, global set to FALSE.

    ## Warning in polychoric(r, correct = correct, weight = weight): 3 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

``` r
res_5 = fa(dep_efa, 5, cor = "poly", rotate = "geominQ", fm = "wls")
```

    ## Warning in polychoric(r, correct = correct, weight = weight): The items do not
    ## have an equal number of response alternatives, global set to FALSE.

    ## Warning in polychoric(r, correct = correct, weight = weight): 3 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

``` r
res_6 = fa(dep_efa, 6, cor = "poly", rotate = "geominQ", fm = "wls")
```

    ## Warning in polychoric(r, correct = correct, weight = weight): The items do not
    ## have an equal number of response alternatives, global set to FALSE.

    ## Warning in polychoric(r, correct = correct, weight = weight): 3 cells were
    ## adjusted for 0 values using the correction for continuity. Examine your data
    ## carefully.

``` r
res_3$loadings
```

    ## 
    ## Loadings:
    ##                           WLS1   WLS2   WLS3  
    ## work_interfere                          -0.239
    ## remote_work               -0.347        -0.222
    ## tech_company              -0.384        -0.277
    ## benefits                   0.774        -0.112
    ## wellness_program           0.932              
    ## mental_health_consequence -0.118  0.266  0.436
    ## phys_health_consequence           0.315  0.139
    ## coworkers                         0.586       
    ## supervisor                 0.182  0.585 -0.107
    ## mental_health_interview                  0.248
    ## phys_health_interview             0.141  0.141
    ## obs_consequence                  -0.149  0.576
    ## 
    ##                 WLS1  WLS2  WLS3
    ## SS loadings    1.806 0.910 0.832
    ## Proportion Var 0.150 0.076 0.069
    ## Cumulative Var 0.150 0.226 0.296

``` r
res_4$loadings
```

    ## 
    ## Loadings:
    ##                           WLS1   WLS2   WLS3   WLS4  
    ## work_interfere                                 -0.284
    ## remote_work               -0.346               -0.196
    ## tech_company              -0.382               -0.283
    ## benefits                   0.764               -0.126
    ## wellness_program           0.937                     
    ## mental_health_consequence -0.125         0.261  0.367
    ## phys_health_consequence                  0.298       
    ## coworkers                                0.609       
    ## supervisor                 0.163 -0.127  0.550       
    ## mental_health_interview           0.962              
    ## phys_health_interview             0.404  0.172       
    ## obs_consequence                         -0.103  0.672
    ## 
    ##                 WLS1  WLS2  WLS3  WLS4
    ## SS loadings    1.793 1.116 0.883 0.816
    ## Proportion Var 0.149 0.093 0.074 0.068
    ## Cumulative Var 0.149 0.242 0.316 0.384

``` r
res_5$loadings
```

    ## 
    ## Loadings:
    ##                           WLS1   WLS2   WLS5   WLS3   WLS4  
    ## work_interfere                                  0.863       
    ## remote_work               -0.359         0.112        -0.125
    ## tech_company              -0.401         0.159  0.130       
    ## benefits                   0.773                            
    ## wellness_program           0.896                            
    ## mental_health_consequence -0.107  0.286        -0.136       
    ## phys_health_consequence           0.991                     
    ## coworkers                                0.607         0.162
    ## supervisor                 0.194         0.621              
    ## mental_health_interview                                0.643
    ## phys_health_interview                    0.245         0.633
    ## obs_consequence            0.105        -0.323 -0.344       
    ## 
    ##                 WLS1  WLS2  WLS5  WLS3  WLS4
    ## SS loadings    1.760 1.069 0.965 0.905 0.867
    ## Proportion Var 0.147 0.089 0.080 0.075 0.072
    ## Cumulative Var 0.147 0.236 0.316 0.392 0.464

``` r
res_6$loadings
```

    ## 
    ## Loadings:
    ##                           WLS1   WLS2   WLS3   WLS4   WLS5   WLS6  
    ## work_interfere                           0.940                     
    ## remote_work               -0.382                      -0.101 -0.148
    ## tech_company              -0.456                             -0.276
    ## benefits                   0.750                             -0.230
    ## wellness_program           0.888                                   
    ## mental_health_consequence         0.338                       0.621
    ## phys_health_consequence           0.917                            
    ## coworkers                                       0.610  0.133       
    ## supervisor                 0.178                0.645              
    ## mental_health_interview                        -0.223  0.548       
    ## phys_health_interview                                  0.695 -0.110
    ## obs_consequence            0.166        -0.227 -0.216         0.355
    ## 
    ##                 WLS1  WLS2  WLS3  WLS4  WLS5  WLS6
    ## SS loadings    1.772 0.962 0.940 0.902 0.817 0.687
    ## Proportion Var 0.148 0.080 0.078 0.075 0.068 0.057
    ## Cumulative Var 0.148 0.228 0.306 0.381 0.449 0.507

### Model Comparison

For the above models in Exploratory factor analyis, we use BIC as the
matrix to identify model with optimal number of factors. As a result,
four-factor model has the smallest BIC which is 31.25.

``` r
BIC = c(res_3$BIC, res_4$BIC, res_5$BIC, res_6$BIC)
model = c("three-factor", "four-factor", "five-factor", "six-factor")


data.frame(model, 
          BIC)
```

    ##          model       BIC
    ## 1 three-factor  78.12328
    ## 2  four-factor  31.24901
    ## 3  five-factor 113.60931
    ## 4   six-factor  62.99408

## Confirmatory Factor Analysis

As the EFA shows, model with four factors has the best performance.
According to the loading result of this model, we choose items with
loading greater than 0.4 to construct the model for Confirmatory Factor
Analysis.<br>

pc1 Company Policy: remote\_work + tech\_company + benefits +
wellness\_program pc2 Health Issue in Interview:
mental\_health\_interview + phys\_health\_interview pc3 Discussion with
Colleagues: phys\_health\_consequence + coworkers + supervisor pc4
Health Discussion Consequence: work\_interfere +
mental\_health\_consequence + obs\_consequence

### Model 1 - no correlated factors

``` r
dep_cfa = features

# in model1, we use the four factor model in EFA 
model1 = 
   "pc1 = ~ remote_work + tech_company + benefits + wellness_program  
    pc2 = ~ mental_health_interview  + phys_health_interview   
    pc3 = ~ phys_health_consequence + coworkers + supervisor
    pc4 = ~ work_interfere + mental_health_consequence + obs_consequence
"

fit_model1 = cfa(model1, data = dep_cfa,std.lv = T, ordered = names(dep_cfa), check.gradient = FALSE, optim.method = "BFGS",
                 optim.force.converged = T, check.post = F)
summary(fit_model1, rsquare = T, fit.measures = T)
```

    ## lavaan 0.6-10 ended normally after 378 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                             BFGS
    ##   Number of model parameters                        38
    ##                                                       
    ##   Number of observations                           544
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                               114.694     115.334
    ##   Degrees of freedom                                48          48
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.045
    ##   Shift parameter                                            5.606
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               666.334     576.730
    ##   Degrees of freedom                                66          66
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.175
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.889       0.868
    ##   Tucker-Lewis Index (TLI)                       0.847       0.819
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.051       0.051
    ##   90 Percent confidence interval - lower         0.039       0.039
    ##   90 Percent confidence interval - upper         0.063       0.063
    ##   P-value RMSEA <= 0.05                          0.449       0.435
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.071       0.071
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   pc1 =~                                              
    ##     remote_work       0.355    0.065    5.452    0.000
    ##     tech_company      0.396    0.073    5.424    0.000
    ##     benefits         -0.748    0.069  -10.848    0.000
    ##     wellness_prgrm   -0.947    0.084  -11.315    0.000
    ##   pc2 =~                                              
    ##     mntl_hlth_ntrv   19.888 3178.787    0.006    0.995
    ##     phys_hlth_ntrv    0.019    2.995    0.006    0.995
    ##   pc3 =~                                              
    ##     phys_hlth_cnsq    0.145    0.065    2.238    0.025
    ##     coworkers         0.343    0.109    3.140    0.002
    ##     supervisor        1.084    0.327    3.312    0.001
    ##   pc4 =~                                              
    ##     work_interfere    0.011    1.911    0.006    0.995
    ##     mntl_hlth_cnsq   -0.007    1.295   -0.006    0.995
    ##     obs_consequenc  -22.835 4020.541   -0.006    0.995
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   pc1 ~~                                              
    ##     pc2              -0.001    0.164   -0.006    0.995
    ##     pc3              -0.056    0.059   -0.948    0.343
    ##     pc4               0.006    1.042    0.006    0.995
    ##   pc2 ~~                                              
    ##     pc3              -0.007    1.166   -0.006    0.995
    ##     pc4              -0.000    0.073   -0.004    0.997
    ##   pc3 ~~                                              
    ##     pc4               0.007    1.318    0.006    0.995
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .remote_work       0.000                           
    ##    .tech_company      0.000                           
    ##    .benefits          0.000                           
    ##    .wellness_prgrm    0.000                           
    ##    .mntl_hlth_ntrv    0.000                           
    ##    .phys_hlth_ntrv    0.000                           
    ##    .phys_hlth_cnsq    0.000                           
    ##    .coworkers         0.000                           
    ##    .supervisor        0.000                           
    ##    .work_interfere    0.000                           
    ##    .mntl_hlth_cnsq    0.000                           
    ##    .obs_consequenc    0.000                           
    ##     pc1               0.000                           
    ##     pc2               0.000                           
    ##     pc3               0.000                           
    ##     pc4               0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     remote_work|t1   -0.525    0.057   -9.288    0.000
    ##     tech_compny|t1    0.894    0.062   14.333    0.000
    ##     benefits|t1      -0.005    0.054   -0.086    0.932
    ##     wllnss_prgrm|1   -0.601    0.057  -10.459    0.000
    ##     mntl_hlth_nt|1   -1.890    0.108  -17.443    0.000
    ##     mntl_hlth_nt|2    1.115    0.068   16.427    0.000
    ##     phys_hlth_nt|1   -1.090    0.067  -16.227    0.000
    ##     phys_hlth_nt|2    0.115    0.054    2.141    0.032
    ##     phys_hlth_cn|1   -1.491    0.082  -18.119    0.000
    ##     phys_hlth_cn|2    0.692    0.059   11.781    0.000
    ##     coworkers|t1     -0.860    0.062  -13.946    0.000
    ##     coworkers|t2     -0.237    0.054   -4.365    0.000
    ##     supervisor|t1    -0.261    0.054   -4.792    0.000
    ##     supervisor|t2     0.563    0.057    9.875    0.000
    ##     work_intrfr|t1   -0.972    0.064  -15.163    0.000
    ##     work_intrfr|t2   -0.387    0.055   -7.007    0.000
    ##     work_intrfr|t3    0.950    0.064   14.940    0.000
    ##     mntl_hlth_cn|1   -0.536    0.057   -9.456    0.000
    ##     mntl_hlth_cn|2    0.348    0.055    6.327    0.000
    ##     obs_consqnc|t1   -0.808    0.061  -13.315    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .remote_work       0.874                           
    ##    .tech_company      0.843                           
    ##    .benefits          0.440                           
    ##    .wellness_prgrm    0.103                           
    ##    .mntl_hlth_ntrv -394.548                           
    ##    .phys_hlth_ntrv    1.000                           
    ##    .phys_hlth_cnsq    0.979                           
    ##    .coworkers         0.883                           
    ##    .supervisor       -0.175                           
    ##    .work_interfere    1.000                           
    ##    .mntl_hlth_cnsq    1.000                           
    ##    .obs_consequenc -520.417                           
    ##     pc1               1.000                           
    ##     pc2               1.000                           
    ##     pc3               1.000                           
    ##     pc4               1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     remote_work       1.000                           
    ##     tech_company      1.000                           
    ##     benefits          1.000                           
    ##     wellness_prgrm    1.000                           
    ##     mntl_hlth_ntrv    1.000                           
    ##     phys_hlth_ntrv    1.000                           
    ##     phys_hlth_cnsq    1.000                           
    ##     coworkers         1.000                           
    ##     supervisor        1.000                           
    ##     work_interfere    1.000                           
    ##     mntl_hlth_cnsq    1.000                           
    ##     obs_consequenc    1.000                           
    ## 
    ## R-Square:
    ##                    Estimate
    ##     remote_work       0.126
    ##     tech_company      0.157
    ##     benefits          0.560
    ##     wellness_prgrm    0.897
    ##     mntl_hlth_ntrv       NA
    ##     phys_hlth_ntrv    0.000
    ##     phys_hlth_cnsq    0.021
    ##     coworkers         0.117
    ##     supervisor           NA
    ##     work_interfere    0.000
    ##     mntl_hlth_cnsq    0.000
    ##     obs_consequenc       NA

### Is there any modification indices?

As the result shows, the error terms of physical health consequence and
mental health consequence are correlated with each other with the
modification indices equals to 44.661. In addition, the error terms of
benefits and mental health consequence are also correlated with MI =
9.8.

``` r
modindices(fit_model1, power = T, sort = T)
```

    ##                           lhs op                       rhs     mi      epc
    ## 173   phys_health_consequence ~~ mental_health_consequence 44.661    0.263
    ## 140                  benefits ~~          wellness_program 11.229    1.289
    ## 147                  benefits ~~ mental_health_consequence  9.836   -0.187
    ## 90                        pc1 =~           obs_consequence  9.774 -288.029
    ## 87                        pc1 =~                supervisor  9.328   -0.476
    ## 86                        pc1 =~                 coworkers  9.053    0.159
    ## 119               remote_work ~~              tech_company  8.317    0.248
    ## 105                       pc3 =~   mental_health_interview  6.591 -276.598
    ## 106                       pc3 =~     phys_health_interview  6.587    0.260
    ## 89                        pc1 =~ mental_health_consequence  5.637    0.118
    ## 165     phys_health_interview ~~                 coworkers  4.847    0.124
    ## 121               remote_work ~~          wellness_program  4.089    0.286
    ## 153          wellness_program ~~                supervisor  3.968    0.170
    ## 88                        pc1 =~            work_interfere  3.828   -0.098
    ## 154          wellness_program ~~            work_interfere  3.820    0.127
    ## 177                 coworkers ~~ mental_health_consequence  3.816    0.118
    ## 129               remote_work ~~           obs_consequence  3.685   -0.159
    ## 110                       pc4 =~               remote_work  3.685    0.007
    ## 112                       pc4 =~                  benefits  3.571    0.008
    ## 148                  benefits ~~           obs_consequence  3.564   -0.181
    ## 144                  benefits ~~                 coworkers  3.546   -0.122
    ## 83                        pc1 =~   mental_health_interview  3.447   96.074
    ## 84                        pc1 =~     phys_health_interview  3.438   -0.090
    ## 135              tech_company ~~                 coworkers  3.358    0.140
    ## 123               remote_work ~~     phys_health_interview  2.517   -0.100
    ## 152          wellness_program ~~                 coworkers  2.342   -0.105
    ## 111                       pc4 =~              tech_company  2.307    0.006
    ## 139              tech_company ~~           obs_consequence  2.303   -0.134
    ## 118                       pc4 =~                supervisor  2.244   -0.017
    ## 181                supervisor ~~           obs_consequence  2.241    0.389
    ## 169     phys_health_interview ~~           obs_consequence  2.141   -0.117
    ## 115                       pc4 =~     phys_health_interview  2.139    0.005
    ## 114                       pc4 =~   mental_health_interview  2.138   -5.429
    ## 138              tech_company ~~ mental_health_consequence  1.952   -0.092
    ## 104                       pc3 =~          wellness_program  1.943    0.108
    ## 130              tech_company ~~                  benefits  1.799    0.167
    ## 101                       pc3 =~               remote_work  1.751    0.075
    ## 108                       pc3 =~ mental_health_consequence  1.634    0.065
    ## 117                       pc4 =~                 coworkers  1.588    0.005
    ## 178                 coworkers ~~           obs_consequence  1.588   -0.111
    ## 182            work_interfere ~~ mental_health_consequence  1.554   -0.074
    ## 150          wellness_program ~~     phys_health_interview  1.315    0.076
    ## 162   mental_health_interview ~~ mental_health_consequence  1.119    0.098
    ## 99                        pc2 =~ mental_health_consequence  1.117    0.005
    ## 163   mental_health_interview ~~           obs_consequence  1.101   73.533
    ## 102                       pc3 =~              tech_company  1.101    0.066
    ## 164     phys_health_interview ~~   phys_health_consequence  1.075    0.070
    ## 155          wellness_program ~~ mental_health_consequence  1.031   -0.069
    ## 184 mental_health_consequence ~~           obs_consequence  0.994  102.573
    ## 167     phys_health_interview ~~            work_interfere  0.924   -0.048
    ## 183            work_interfere ~~           obs_consequence  0.849 -168.423
    ## 134              tech_company ~~   phys_health_consequence  0.835    0.063
    ## 91                        pc2 =~               remote_work  0.824   -0.003
    ## 122               remote_work ~~   mental_health_interview  0.819   -0.068
    ## 109                       pc3 =~           obs_consequence  0.809  -94.173
    ## 131              tech_company ~~          wellness_program  0.777    0.137
    ## 126               remote_work ~~                supervisor  0.702    0.055
    ## 128               remote_work ~~ mental_health_consequence  0.679    0.054
    ## 142                  benefits ~~     phys_health_interview  0.626    0.048
    ## 133              tech_company ~~     phys_health_interview  0.607   -0.056
    ## 127               remote_work ~~            work_interfere  0.498   -0.042
    ## 146                  benefits ~~            work_interfere  0.442    0.040
    ## 171   phys_health_consequence ~~                supervisor  0.383   -0.135
    ## 85                        pc1 =~   phys_health_consequence  0.378    0.032
    ## 149          wellness_program ~~   mental_health_interview  0.373   -0.069
    ## 94                        pc2 =~          wellness_program  0.371   -0.003
    ## 170   phys_health_consequence ~~                 coworkers  0.352    0.047
    ## 174   phys_health_consequence ~~           obs_consequence  0.328   -0.035
    ## 116                       pc4 =~   phys_health_consequence  0.325    0.002
    ## 125               remote_work ~~                 coworkers  0.289    0.035
    ## 160   mental_health_interview ~~                supervisor  0.138   -0.076
    ## 97                        pc2 =~                supervisor  0.137   -0.004
    ## 166     phys_health_interview ~~                supervisor  0.126    0.031
    ## 124               remote_work ~~   phys_health_consequence  0.119    0.023
    ## 180                supervisor ~~ mental_health_consequence  0.107   -0.019
    ## 120               remote_work ~~                  benefits  0.098    0.035
    ## 137              tech_company ~~            work_interfere  0.095    0.021
    ## 176                 coworkers ~~            work_interfere  0.094   -0.017
    ## 96                        pc2 =~                 coworkers  0.081    0.001
    ## 100                       pc2 =~           obs_consequence  0.081   -1.779
    ## 159   mental_health_interview ~~                 coworkers  0.080    0.018
    ## 151          wellness_program ~~   phys_health_consequence  0.068   -0.019
    ## 145                  benefits ~~                supervisor  0.062    0.018
    ## 95                        pc2 =~   phys_health_consequence  0.061    0.001
    ## 158   mental_health_interview ~~   phys_health_consequence  0.061    0.019
    ## 136              tech_company ~~                supervisor  0.054    0.017
    ## 103                       pc3 =~                  benefits  0.037   -0.012
    ## 161   mental_health_interview ~~            work_interfere  0.036    0.012
    ## 98                        pc2 =~            work_interfere  0.036    0.001
    ## 175                 coworkers ~~                supervisor  0.023    0.114
    ## 93                        pc2 =~                  benefits  0.014    0.001
    ## 141                  benefits ~~   mental_health_interview  0.014    0.011
    ## 168     phys_health_interview ~~ mental_health_consequence  0.005    0.004
    ## 107                       pc3 =~            work_interfere  0.003   -0.003
    ## 179                supervisor ~~            work_interfere  0.003   -0.003
    ## 113                       pc4 =~          wellness_program  0.001    0.000
    ## 156          wellness_program ~~           obs_consequence  0.001   -0.004
    ## 132              tech_company ~~   mental_health_interview  0.001    0.003
    ## 92                        pc2 =~              tech_company  0.001    0.000
    ## 143                  benefits ~~   phys_health_consequence  0.000   -0.001
    ## 172   phys_health_consequence ~~            work_interfere  0.000    0.001
    ##     sepc.all delta      ncp power decision
    ## 173    0.266   0.1    6.441 0.718  **(m)**
    ## 140    6.062   0.1    0.068 0.058  **(m)**
    ## 147   -0.282   0.1    2.812 0.389  **(m)**
    ## 90  -288.029   0.1    0.000 0.050  **(m)**
    ## 87    -0.476   0.1    0.412 0.098  **(m)**
    ## 86     0.159   0.1    3.587 0.474  **(m)**
    ## 119    0.289   0.1    1.349 0.213  **(m)**
    ## 105 -276.598   0.1    0.000 0.050  **(m)**
    ## 106    0.260   0.1    0.971 0.167  **(m)**
    ## 89     0.118   0.1    4.036 0.520  **(m)**
    ## 165    0.132   0.1    3.163 0.428  **(m)**
    ## 121    0.953   0.1    0.501 0.109  **(m)**
    ## 153    1.265   0.1    1.377 0.217  **(m)**
    ## 88    -0.098   0.1    3.979 0.514      (i)
    ## 154    0.397   0.1    2.355 0.336      (i)
    ## 177    0.125   0.1    2.762 0.383      (i)
    ## 129   -0.007   0.1    1.456 0.226      (i)
    ## 110    0.007   0.1  758.930 1.000     (nm)
    ## 112    0.008   0.1  565.760 1.000     (nm)
    ## 148   -0.012   0.1    1.085 0.181      (i)
    ## 144   -0.196   0.1    2.380 0.339      (i)
    ## 83    96.074   0.1    0.000 0.050      (i)
    ## 84    -0.090   0.1    4.208 0.536      (i)
    ## 135    0.162   0.1    1.723 0.259      (i)
    ## 123   -0.107   0.1    2.510 0.354      (i)
    ## 152   -0.350   0.1    2.107 0.306      (i)
    ## 111    0.006   0.1  671.371 1.000     (nm)
    ## 139   -0.006   0.1    1.288 0.206      (i)
    ## 118   -0.017   0.1   77.157 1.000     (nm)
    ## 181    0.041   0.1    0.148 0.067      (i)
    ## 169   -0.005   0.1    1.569 0.240      (i)
    ## 115    0.005   0.1  817.349 1.000     (nm)
    ## 114   -5.429   0.1    0.001 0.050      (i)
    ## 138   -0.100   0.1    2.321 0.332      (i)
    ## 104    0.108   0.1    1.665 0.252      (i)
    ## 130    0.274   0.1    0.645 0.127      (i)
    ## 101    0.075   0.1    3.082 0.419      (i)
    ## 108    0.065   0.1    3.867 0.503      (i)
    ## 117    0.005   0.1  673.284 1.000     (nm)
    ## 178   -0.005   0.1    1.292 0.206      (i)
    ## 182   -0.074   0.1    2.864 0.395      (i)
    ## 150    0.238   0.1    2.261 0.324      (i)
    ## 162    0.005   0.1    1.164 0.190      (i)
    ## 99     0.005   0.1  460.481 1.000     (nm)
    ## 163    0.162   0.1    0.000 0.050      (i)
    ## 102    0.066   0.1    2.508 0.353      (i)
    ## 164    0.071   0.1    2.166 0.313      (i)
    ## 155   -0.215   0.1    2.168 0.313      (i)
    ## 184    4.496   0.1    0.000 0.050      (i)
    ## 167   -0.048   0.1    3.933 0.509      (i)
    ## 183   -7.383   0.1    0.000 0.050      (i)
    ## 134    0.069   0.1    2.101 0.305      (i)
    ## 91    -0.003   0.1  695.433 1.000     (nm)
    ## 122   -0.004   0.1    1.758 0.264      (i)
    ## 109  -94.173   0.1    0.000 0.050      (i)
    ## 131    0.465   0.1    0.415 0.099      (i)
    ## 126    0.140   0.1    2.327 0.332      (i)
    ## 128    0.057   0.1    2.370 0.337      (i)
    ## 142    0.072   0.1    2.740 0.381      (i)
    ## 133   -0.060   0.1    1.970 0.289      (i)
    ## 127   -0.045   0.1    2.759 0.383      (i)
    ## 146    0.060   0.1    2.817 0.389      (i)
    ## 171   -0.327   0.1    0.209 0.074      (i)
    ## 85     0.032   0.1    3.792 0.495      (i)
    ## 149   -0.011   0.1    0.789 0.144      (i)
    ## 94    -0.003   0.1  311.922 1.000     (nm)
    ## 170    0.051   0.1    1.571 0.241      (i)
    ## 174   -0.002   0.1    2.643 0.369      (i)
    ## 116    0.002   0.1 1378.051 1.000     (nm)
    ## 125    0.040   0.1    2.316 0.331      (i)
    ## 160   -0.009   0.1    0.242 0.078      (i)
    ## 97    -0.004   0.1   95.533 1.000     (nm)
    ## 166    0.075   0.1    1.272 0.204      (i)
    ## 124    0.025   0.1    2.173 0.314      (i)
    ## 180   -0.046   0.1    2.943 0.404      (i)
    ## 120    0.056   0.1    0.810 0.147      (i)
    ## 137    0.023   0.1    2.156 0.312      (i)
    ## 176   -0.018   0.1    3.376 0.451      (i)
    ## 96     0.001   0.1  946.715 1.000     (nm)
    ## 100   -1.779   0.1    0.000 0.050      (i)
    ## 159    0.001   0.1    2.394 0.340      (i)
    ## 151   -0.058   0.1    1.999 0.293      (i)
    ## 145    0.066   0.1    1.874 0.278      (i)
    ## 95     0.001   0.1  661.899 1.000     (nm)
    ## 158    0.001   0.1    1.674 0.253      (i)
    ## 136    0.044   0.1    1.906 0.282      (i)
    ## 103   -0.012   0.1    2.389 0.339      (i)
    ## 161    0.001   0.1    2.426 0.344      (i)
    ## 98     0.001   0.1  959.427 1.000     (nm)
    ## 175    0.289   0.1    0.018 0.052      (i)
    ## 93     0.001   0.1  473.243 1.000     (nm)
    ## 141    0.001   0.1    1.196 0.194      (i)
    ## 168    0.004   0.1    3.193 0.431      (i)
    ## 107   -0.003   0.1    3.450 0.459      (i)
    ## 179   -0.007   0.1    2.749 0.381      (i)
    ## 113    0.000   0.1  388.461 1.000     (nm)
    ## 156   -0.001   0.1    0.745 0.139      (i)
    ## 132    0.000   0.1    1.120 0.185      (i)
    ## 92     0.000   0.1  442.910 1.000     (nm)
    ## 143   -0.002   0.1    2.484 0.351      (i)
    ## 172    0.001   0.1    2.959 0.405      (i)

### Model 2 - error term added

In model 2, we added the correlated error term to model11.

``` r
model2 = 
   "# latent variables definition
    pc1 = ~ remote_work + tech_company + benefits + wellness_program  
    pc2 = ~ mental_health_interview  + phys_health_interview   
    pc3 = ~ phys_health_consequence + coworkers + supervisor
    pc4 = ~ work_interfere + mental_health_consequence + obs_consequence
    
    #residual covariance
    phys_health_consequence ~~ mental_health_consequence
    mental_health_consequence ~~ benefits
"

fit_model2 = cfa(model2, data = dep_cfa,std.lv = T, ordered = names(dep_cfa), check.gradient = FALSE, optim.method = "BFGS",
                 optim.force.converged = T, check.post = F)
summary(fit_model2, rsquare = T, fit.measures = T)
```

    ## lavaan 0.6-10 ended normally after 386 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                             BFGS
    ##   Number of model parameters                        40
    ##                                                       
    ##   Number of observations                           544
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                60.541      63.540
    ##   Degrees of freedom                                46          46
    ##   P-value (Chi-square)                           0.074       0.044
    ##   Scaling correction factor                                  1.037
    ##   Shift parameter                                            5.184
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               666.334     576.730
    ##   Degrees of freedom                                66          66
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.175
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.976       0.966
    ##   Tucker-Lewis Index (TLI)                       0.965       0.951
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.024       0.026
    ##   90 Percent confidence interval - lower         0.000       0.005
    ##   90 Percent confidence interval - upper         0.039       0.041
    ##   P-value RMSEA <= 0.05                          0.999       0.997
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.061       0.061
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   pc1 =~                                              
    ##     remote_work       0.355    0.065    5.452    0.000
    ##     tech_company      0.396    0.073    5.424    0.000
    ##     benefits         -0.749    0.069  -10.848    0.000
    ##     wellness_prgrm   -0.947    0.084  -11.315    0.000
    ##   pc2 =~                                              
    ##     mntl_hlth_ntrv   23.701 5380.296    0.004    0.996
    ##     phys_hlth_ntrv    0.016    3.569    0.004    0.996
    ##   pc3 =~                                              
    ##     phys_hlth_cnsq    0.145    0.065    2.240    0.025
    ##     coworkers         0.343    0.109    3.143    0.002
    ##     supervisor        1.083    0.327    3.316    0.001
    ##   pc4 =~                                              
    ##     work_interfere    0.011    1.844    0.006    0.995
    ##     mntl_hlth_cnsq   -0.008    1.250   -0.006    0.995
    ##     obs_consequenc  -21.952 3585.327   -0.006    0.995
    ## 
    ## Covariances:
    ##                              Estimate  Std.Err  z-value  P(>|z|)
    ##  .phys_health_consequence ~~                                    
    ##    .mntl_hlth_cnsq              0.263    0.039    6.670    0.000
    ##  .benefits ~~                                                   
    ##    .mntl_hlth_cnsq             -0.182    0.059   -3.100    0.002
    ##   pc1 ~~                                                        
    ##     pc2                        -0.001    0.195   -0.004    0.996
    ##     pc3                        -0.056    0.059   -0.948    0.343
    ##     pc4                         0.006    1.005    0.006    0.995
    ##   pc2 ~~                                                        
    ##     pc3                        -0.006    1.390   -0.004    0.996
    ##     pc4                        -0.000    0.075   -0.003    0.997
    ##   pc3 ~~                                                        
    ##     pc4                         0.008    1.273    0.006    0.995
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .remote_work       0.000                           
    ##    .tech_company      0.000                           
    ##    .benefits          0.000                           
    ##    .wellness_prgrm    0.000                           
    ##    .mntl_hlth_ntrv    0.000                           
    ##    .phys_hlth_ntrv    0.000                           
    ##    .phys_hlth_cnsq    0.000                           
    ##    .coworkers         0.000                           
    ##    .supervisor        0.000                           
    ##    .work_interfere    0.000                           
    ##    .mntl_hlth_cnsq    0.000                           
    ##    .obs_consequenc    0.000                           
    ##     pc1               0.000                           
    ##     pc2               0.000                           
    ##     pc3               0.000                           
    ##     pc4               0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     remote_work|t1   -0.525    0.057   -9.288    0.000
    ##     tech_compny|t1    0.894    0.062   14.333    0.000
    ##     benefits|t1      -0.005    0.054   -0.086    0.932
    ##     wllnss_prgrm|1   -0.601    0.057  -10.459    0.000
    ##     mntl_hlth_nt|1   -1.890    0.108  -17.443    0.000
    ##     mntl_hlth_nt|2    1.115    0.068   16.427    0.000
    ##     phys_hlth_nt|1   -1.090    0.067  -16.227    0.000
    ##     phys_hlth_nt|2    0.115    0.054    2.141    0.032
    ##     phys_hlth_cn|1   -1.491    0.082  -18.119    0.000
    ##     phys_hlth_cn|2    0.692    0.059   11.781    0.000
    ##     coworkers|t1     -0.860    0.062  -13.946    0.000
    ##     coworkers|t2     -0.237    0.054   -4.365    0.000
    ##     supervisor|t1    -0.261    0.054   -4.792    0.000
    ##     supervisor|t2     0.563    0.057    9.875    0.000
    ##     work_intrfr|t1   -0.972    0.064  -15.163    0.000
    ##     work_intrfr|t2   -0.387    0.055   -7.007    0.000
    ##     work_intrfr|t3    0.950    0.064   14.940    0.000
    ##     mntl_hlth_cn|1   -0.536    0.057   -9.456    0.000
    ##     mntl_hlth_cn|2    0.348    0.055    6.327    0.000
    ##     obs_consqnc|t1   -0.808    0.061  -13.315    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .remote_work       0.874                           
    ##    .tech_company      0.843                           
    ##    .benefits          0.440                           
    ##    .wellness_prgrm    0.103                           
    ##    .mntl_hlth_ntrv -560.760                           
    ##    .phys_hlth_ntrv    1.000                           
    ##    .phys_hlth_cnsq    0.979                           
    ##    .coworkers         0.882                           
    ##    .supervisor       -0.174                           
    ##    .work_interfere    1.000                           
    ##    .mntl_hlth_cnsq    1.000                           
    ##    .obs_consequenc -480.876                           
    ##     pc1               1.000                           
    ##     pc2               1.000                           
    ##     pc3               1.000                           
    ##     pc4               1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     remote_work       1.000                           
    ##     tech_company      1.000                           
    ##     benefits          1.000                           
    ##     wellness_prgrm    1.000                           
    ##     mntl_hlth_ntrv    1.000                           
    ##     phys_hlth_ntrv    1.000                           
    ##     phys_hlth_cnsq    1.000                           
    ##     coworkers         1.000                           
    ##     supervisor        1.000                           
    ##     work_interfere    1.000                           
    ##     mntl_hlth_cnsq    1.000                           
    ##     obs_consequenc    1.000                           
    ## 
    ## R-Square:
    ##                    Estimate
    ##     remote_work       0.126
    ##     tech_company      0.157
    ##     benefits          0.560
    ##     wellness_prgrm    0.897
    ##     mntl_hlth_ntrv       NA
    ##     phys_hlth_ntrv    0.000
    ##     phys_hlth_cnsq    0.021
    ##     coworkers         0.118
    ##     supervisor           NA
    ##     work_interfere    0.000
    ##     mntl_hlth_cnsq    0.000
    ##     obs_consequenc       NA

### Model comparison - before and after mi

By comparing the model1 and model2, model2 added two error term
correlation. Model1 has a chi-squared test with p-value &lt; 0.001,
which indicates bad fit. However, model2 with a chisquared test p-value
= 0.074, indicating good fit. On the other hand, model2 also performs
better according to its CFI(0.976), TLI(0.965) and RSMEA(0.024).
Therefore, we decided to use model2 for the later structural equation
model.

``` r
metrics = c("RSMEA", "CFI", "TLI", "Chi-square p-value", "df")
model1 = c(0.051, 0.889, 0.847, 0.000, 48)
model2 = c(0.024, 0.976, 0.965, 0.074, 46)

knitr::kable(data.frame(metrics, model1, model2)) 
```

| metrics            | model1 | model2 |
|:-------------------|-------:|-------:|
| RSMEA              |  0.051 |  0.024 |
| CFI                |  0.889 |  0.976 |
| TLI                |  0.847 |  0.965 |
| Chi-square p-value |  0.000 |  0.074 |
| df                 | 48.000 | 46.000 |
