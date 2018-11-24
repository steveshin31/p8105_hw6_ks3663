hw6\_Linear Models
================
Kee-Young Shin
November 23, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ---------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(RCurl)
```

    ## Loading required package: bitops

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

Problem 1
---------

``` r
# import data
homicide_df = read.csv(text = 
    getURL("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"))
```

``` r
# clean and wrangle data 
homicide_df = homicide_df %>% 
  unite(city_state, city:state, sep = ", ") %>% 
  janitor::clean_names() %>% 
  mutate(binary_solved = ifelse(disposition == "Closed by arrest", 1, 0),
         victim_age = as.numeric(victim_age),
         victim_race = as.factor(ifelse(victim_race == "White", "white", "non-white")),
         victim_race = relevel(victim_race, ref = "white")) %>% 
  filter(!(city_state == "Phoenix, AZ")) %>% 
  filter(!(city_state == "Dallas, TX")) %>% 
  filter(!(city_state == "Kansas City, MO")) %>% 
  filter(!(city_state == "Tulsa, AL"))
```
