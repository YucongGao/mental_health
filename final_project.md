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

### Convert data to categories

age: (-Inf,20\] \~ 1, (20,35\] \~ 2, (35,65\] \~ 3, (65, Inf\] \~ 4
gender: male \~ 1, female \~ 2, trans \~ 3 country: United State \~ 1,
others \~ 0 family\_history: yes \~ 1, no \~ 0 work\_inference: never \~
1, rarely \~ 2, sometimes \~ 3, often \~ 4 no\_employees: 1-5 \~ 1, 6-25
\~ 2, 26-100 \~ 3, 100-500 \~ 4, 500-1000 \~ 5, more than 1000 \~ 6
remote\_work: yes \~ 1, no \~ 0 tech\_company: yes \~ 1, no \~ 0
benefits: yes \~ 1, no \~ 2, don’t know \~ 3 care\_options: yes \~ 1, no
\~ 2, TRUE \~ 3 wellness\_program: yes \~ 1, no \~ 2, don’t know \~ 3
seek\_help: yes \~ 1, no \~ 2, don’t know \~ 3 anonymity: yes \~ 1, no
\~ 2, don’t know \~ 3 leave: Very easy \~ 1, Somewhat easy \~ 2,
Somewhat difficult \~ 3, Very difficult \~ 4, Don’t know \~ 5
mental\_health\_consequence: Yes \~ 1, No \~ 2, Maybe \~ 3
phys\_health\_consequence: Yes \~ 1, No \~ 2, Maybe \~ 3 coworkers: Yes
\~ 1, No \~ 2, Some of them \~ 3 supervisor: Yes \~ 1, No \~ 2, Some of
them \~ 3 mental\_health\_interview: Yes \~ 1, No \~ 2, Maybe \~ 3
phys\_health\_interview: Yes \~ 1, No \~ 2, Maybe \~ 3
mental\_vs\_physical: yes \~ 1, no \~ 2, don’t know \~ 3
obs\_consequence: Yes \~ 1, No \~ 0

``` r
dat = tibble(data.origin) %>% janitor::clean_names()

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
          care_options = factor(case_when(care_options == "Yes" ~ 1, 
                                          care_options == "No" ~ 2,
                                          T ~ 3)), 
          wellness_program = factor(case_when(wellness_program == "Yes" ~ 1, 
                                              wellness_program == "No" ~ 2,
                                              TRUE ~ 3)), 
          seek_help = factor(case_when(seek_help == "Yes" ~ 1, 
                                       seek_help == "No" ~ 2,
                                       TRUE ~ 3)), 
          anonymity = factor(case_when(anonymity == "Yes" ~ 1, 
                                       anonymity == "No" ~ 2,
                                       TRUE ~ 3)), 
          leave = factor(case_when(leave == "Very easy" ~ 1, 
                                   leave == "Somewhat easy" ~ 2,
                                   leave == "Somewhat difficult" ~ 3,
                                   leave == "Very difficult" ~ 4,
                                   TRUE ~ 5)), 
          
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
          mental_vs_physical = factor(case_when(mental_vs_physical == "Yes" ~ 1, 
                                       mental_vs_physical == "No" ~ 2,
                                       TRUE ~ 3)), 
          obs_consequence = factor(case_when(obs_consequence == "Yes" ~ 1, 
                                             TRUE ~ 2)))
```
