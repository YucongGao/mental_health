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
