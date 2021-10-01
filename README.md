ST558 - Vignette Project - Covid-19 Data
================
Jasmine Wang
10/05/2021

-   [Getting an API](#getting-an-api)
-   [Welcome to GitHub Pages](#welcome-to-github-pages)
    -   [Markdown](#markdown)
    -   [Jekyll Themes](#jekyll-themes)
    -   [Support or Contact](#support-or-contact)
-   [R Markdown](#r-markdown)

## Getting an API

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

# url <- "https://api.covid19api.com/country/united-states?from=2020-03-01T00:00:00Z&to=2020-04-01T00:00:00Z"
dec_1 <- GET("https://api.covid19api.com/country/united-states?from=2020-12-31T00:00:00Z&to=2020-12-31T23:59:59Z")
#str(usdat, max.level = 2)
jan_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-01-31T00:00:00Z&to=2021-01-31T23:59:59Z")
feb_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-02-28T00:00:00Z&to=2021-02-28T23:59:59Z")
mar_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-03-31T00:00:00Z&to=2021-03-31T23:59:59Z")
apr_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-04-30T00:00:00Z&to=2021-04-30T23:59:59Z")
may_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-05-31T00:00:00Z&to=2021-05-31T23:59:59Z")
jun_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-06-30T00:00:00Z&to=2021-06-30T23:59:59Z")
jul_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-07-31T00:00:00Z&to=2021-07-31T23:59:59Z")
aug_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-08-31T00:00:00Z&to=2021-08-31T23:59:59Z")
sep_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-09-30T00:00:00Z&to=2021-09-30T23:59:59Z")

dec_2 <- rawToChar(dec_1$content)
jan_2 <- rawToChar(jan_1$content)
feb_2 <- rawToChar(feb_1$content)
mar_2 <- rawToChar(mar_1$content)
apr_2 <- rawToChar(apr_1$content)
may_2 <- rawToChar(may_1$content)
jun_2 <- rawToChar(jun_1$content)
jul_2 <- rawToChar(jul_1$content)
aug_2 <- rawToChar(aug_1$content)
sep_2 <- rawToChar(sep_1$content)

dec_3 <- fromJSON(dec_2)
jan_3 <- fromJSON(jan_2)
feb_3 <- fromJSON(feb_2)
mar_3 <- fromJSON(mar_2)
apr_3 <- fromJSON(apr_2)
may_3 <- fromJSON(may_2)
jun_3 <- fromJSON(jun_2)
jul_3 <- fromJSON(jul_2)
aug_3 <- fromJSON(aug_2)
sep_3 <- fromJSON(sep_2)

dec_4 <- dec_3 %>% select(Province, Deaths, Active, Date) 
jan_4 <- jan_3 %>% select(Province, Deaths, Active, Date)
feb_4 <- feb_3 %>% select(Province, Deaths, Active, Date)
mar_4 <- mar_3 %>% select(Province, Deaths, Active, Date)
apr_4 <- apr_3 %>% select(Province, Deaths, Active, Date)
may_4 <- may_3 %>% select(Province, Deaths, Active, Date)
jun_4 <- jun_3 %>% select(Province, Deaths, Active, Date)
jul_4 <- jul_3 %>% select(Province, Deaths, Active, Date)
aug_4 <- aug_3 %>% select(Province, Deaths, Active, Date)
sep_4 <- sep_3 %>% select(Province, Deaths, Active, Date)

data_1 <- rbind(jan_4, feb_4, mar_4, apr_4, may_4, jun_4, jul_4, aug_4, sep_4)
data_2 <- rbind(dec_4, jan_4, feb_4, mar_4, apr_4, may_4, jun_4, jul_4, aug_4)

data_11 <- data_1 %>% group_by(Province, Date) %>% summarise(across(c(Deaths, Active), sum))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
data_11
```

    ## # A tibble: 531 x 4
    ##    Province  Date                 Deaths   Active
    ##    <chr>     <chr>                 <int>    <int>
    ##  1 ""        2021-01-31T00:00:00Z 449195 25799922
    ##  2 ""        2021-02-28T00:00:00Z 514818 28136320
    ##  3 ""        2021-03-31T00:00:00Z 552322 29912496
    ##  4 ""        2021-04-30T00:00:00Z 576112 31774444
    ##  5 ""        2021-05-31T00:00:00Z 594286 32673092
    ##  6 ""        2021-06-30T00:00:00Z 604613 33060180
    ##  7 ""        2021-07-31T00:00:00Z 613352 34365119
    ##  8 ""        2021-08-31T00:00:00Z 641180 38557964
    ##  9 ""        2021-09-30T00:00:00Z 697851 42761967
    ## 10 "Alabama" 2021-01-31T00:00:00Z   7688   451951
    ## # ... with 521 more rows

``` r
data_22 <- data_2 %>% group_by(Province, Date) %>% summarise(across(c(Deaths, Active), sum))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
names(data_22) <- c("State", "Date2", "Deaths2", "Active2")
mydata1 <- cbind(data_11, data_22)
mydata1
```

    ## # A tibble: 531 x 8
    ##    Province  Date                 Deaths   Active State     Date2                Deaths2  Active2
    ##    <chr>     <chr>                 <int>    <int> <chr>     <chr>                  <int>    <int>
    ##  1 ""        2021-01-31T00:00:00Z 449195 25799922 ""        2020-12-31T00:00:00Z  352001 19748034
    ##  2 ""        2021-02-28T00:00:00Z 514818 28136320 ""        2021-01-31T00:00:00Z  449195 25799922
    ##  3 ""        2021-03-31T00:00:00Z 552322 29912496 ""        2021-02-28T00:00:00Z  514818 28136320
    ##  4 ""        2021-04-30T00:00:00Z 576112 31774444 ""        2021-03-31T00:00:00Z  552322 29912496
    ##  5 ""        2021-05-31T00:00:00Z 594286 32673092 ""        2021-04-30T00:00:00Z  576112 31774444
    ##  6 ""        2021-06-30T00:00:00Z 604613 33060180 ""        2021-05-31T00:00:00Z  594286 32673092
    ##  7 ""        2021-07-31T00:00:00Z 613352 34365119 ""        2021-06-30T00:00:00Z  604613 33060180
    ##  8 ""        2021-08-31T00:00:00Z 641180 38557964 ""        2021-07-31T00:00:00Z  613352 34365119
    ##  9 ""        2021-09-30T00:00:00Z 697851 42761967 ""        2021-08-31T00:00:00Z  641180 38557964
    ## 10 "Alabama" 2021-01-31T00:00:00Z   7688   451951 "Alabama" 2020-12-31T00:00:00Z    4827   356399
    ## # ... with 521 more rows

``` r
mydata2 <- mydata1 %>% mutate(Death = Deaths - Deaths2, Cases = Active - Active2, Total = Death + Cases, 
                              Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>% 
  select(State, Month, Death, Cases, Total)
```

    ## Adding missing grouping variables: `Province`

``` r
`%!in%` <- Negate(`%in%`)

mydata3 <- mydata2 %>% filter(Province %!in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands")) %>% 
  mutate(f500_deaths = if_else(Death < 500, "Less than 500 deaths", "More than 500 deaths"), 
         Total_cases = if_else(Total < 9000, "1. Less than 9000", 
                             if_else(Total < 30000, "2. Between 9000 and 30,000", "3. More than 30,000")), 
         vaccine = if_else(Month %in% c("Jan", "Feb", "Mar", "Apr"), "1. Vaccine-ing", 
                           if_else(Month %in% c("May", "Jun", "Jul"), "2. Some vaccined", "3. Back to school")))
dim(mydata3)
```

    ## [1] 468   9

``` r
names(mydata3)
```

    ## [1] "Province"    "State"       "Month"       "Death"       "Cases"       "Total"       "f500_deaths" "Total_cases" "vaccine"

``` r
# 1st contingency table
table(mydata3$Total_cases, mydata3$vaccine)
```

    ##                             
    ##                              1. Vaccine-ing 2. Some vaccined 3. Back to school
    ##   1. Less than 9000                      46               79                10
    ##   2. Between 9000 and 30,000             63               51                24
    ##   3. More than 30,000                    99               26                70

``` r
# 2nd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Death) %>%
  pivot_wider(names_from = "Month", values_from = "Death") %>% arrange(desc(Jan))
```

    ## # A tibble: 6 x 10
    ##   Province         Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep
    ##   <chr>          <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ## 1 California     14994 11621  6373  2463  1263   813   888  1448  3257
    ## 2 Texas           9008  6623  4555  1967  1289   829   940  3767  8159
    ## 3 New York        5651  3967  2680  1958  1043   372   171   613  1104
    ## 4 Florida         4806  4373  2573  1736  1613   998  1307  5482 10448
    ## 5 North Carolina  2587  1877   900   539   427   359   198   833  2056
    ## 6 Michigan        2489   977   610  1629  1627   620   187   359   808

``` r
# 3rd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Total) %>%
  pivot_wider(names_from = "Month", values_from = "Total") %>% arrange(desc(Jan))
```

    ## # A tibble: 6 x 10
    ##   Province          Jan    Feb    Mar    Apr   May   Jun    Jul    Aug    Sep
    ##   <chr>           <int>  <int>  <int>  <int> <int> <int>  <int>  <int>  <int>
    ## 1 California     998625 243329 101168  73358 47452 26986 144717 399599 359546
    ## 2 Texas          603560 276669 138897 102018 59481 46660 138768 473409 456018
    ## 3 New York       441133 223906 229278 175034 54270 12584  35616 128220 148119
    ## 4 Florida        398062 187844 148514 175783 87300 44646 268770 616220 347425
    ## 5 North Carolina 217981 101022  55584  55620 31402 12535  34387 165578 181600
    ## 6 Michigan        78775  34297  99685 189525 55110  8209  11064  49187  89646

``` r
# summaries
mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_total = mean(Total), sd_total = sd(Total), median_total = median(Total), IQR_total = IQR(Total))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

    ## # A tibble: 156 x 6
    ##    Province  vaccine           avg_total sd_total median_total IQR_total
    ##    <chr>     <chr>                 <dbl>    <dbl>        <dbl>     <dbl>
    ##  1 ""        1. Vaccine-ing     3062630. 2074250.     2143880.  1471063.
    ##  2 ""        2. Some vaccined    875972.  459495.      916822    458132.
    ##  3 ""        3. Back to school  4240674.   28285.     4240674.    20000.
    ##  4 "Alabama" 1. Vaccine-ing       41674    38795.       27874.    30078.
    ##  5 "Alabama" 2. Some vaccined     19228.   13907.       15483     13523 
    ##  6 "Alabama" 3. Back to school   105434    12287.      105434      8688 
    ##  7 "Alaska"  1. Vaccine-ing        5284.    1468.        4926      1329 
    ##  8 "Alaska"  2. Some vaccined      2446     1607.        2060      1572 
    ##  9 "Alaska"  3. Back to school    19590.    9233.       19590.     6528.
    ## 10 "Arizona" 1. Vaccine-ing       85572.  103133.       41704.    79390.
    ## # ... with 146 more rows

``` r
mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_death = mean(Death), sd_death = sd(Death), median_death = median(Death), IQR_death = IQR(Death))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

    ## # A tibble: 156 x 6
    ##    Province  vaccine           avg_death sd_death median_death IQR_death
    ##    <chr>     <chr>                 <dbl>    <dbl>        <dbl>     <dbl>
    ##  1 ""        1. Vaccine-ing      56028.  32502.         51564.   39440. 
    ##  2 ""        2. Some vaccined    12413.   5052.         10327     4718. 
    ##  3 ""        3. Back to school   42250.  20395.         42250.   14422. 
    ##  4 "Alabama" 1. Vaccine-ing       1517.   1226.          1433     1842. 
    ##  5 "Alabama" 2. Some vaccined      213.     33.6          206       33  
    ##  6 "Alabama" 3. Back to school    1382.    897.          1382.     634. 
    ##  7 "Alaska"  1. Vaccine-ing         35.2    14.5           31       12.8
    ##  8 "Alaska"  2. Some vaccined       14       7.21          12        7  
    ##  9 "Alaska"  3. Back to school      94      55.2           94       39  
    ## 10 "Arizona" 1. Vaccine-ing       2115    1780.          1924.    2380. 
    ## # ... with 146 more rows

``` r
#dec_5 <- dec_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Dec")
#jan_5 <- jan_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#feb_5 <- feb_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#mar_5 <- mar_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#apr_5 <- apr_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#may_5 <- may_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#jun_5 <- jun_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#jul_5 <- jul_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
#aug_5 <- aug_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")

#rbind(dec_5, jan_5)

#Deaths <- jan_5$Deaths - dec_5$Deaths
#Active <- jan_5$Active - dec_5$Active

#jan_6 <- data.frame(jan_5$Province, dec_5$Deaths - dec_5$Deaths, jan_5$Active - dec_5$Active, "Jan")
#feb_6 <- data.frame(feb_5$Province, jan_5$Deaths - dec_5$Deaths, feb_5$Active - jan_5$Active, "Feb")
#mar_6 <- data.frame(mar_5$Province, feb_5$Deaths - dec_5$Deaths, mar_5$Active - feb_5$Active, "Mar")
#apr_6 <- data.frame(apr_5$Province, mar_5$Deaths - dec_5$Deaths, apr_5$Active - mar_5$Active, "Apr")
#may_6 <- data.frame(may_5$Province, apr_5$Deaths - dec_5$Deaths, may_5$Active - apr_5$Active, "May")
#jun_6 <- data.frame(jun_5$Province, may_5$Deaths - dec_5$Deaths, jun_5$Active - may_5$Active, "Jun")
#jul_6 <- data.frame(jul_5$Province, jun_5$Deaths - dec_5$Deaths, jul_5$Active - jun_5$Active, "Jul")
#aug_6 <- data.frame(aug_5$Province, jul_5$Deaths - dec_5$Deaths, aug_5$Active - jul_5$Active, "Aug")

#colnames(jan_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(feb_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(mar_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(apr_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(may_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(jun_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(jul_6) <- c("State", "Deaths", "Cases", "Month")
#colnames(aug_6) <- c("State", "Deaths", "Cases", "Month")
#rbind(jan_6, feb_6, mar_6, apr_6, may_6, jun_6, jul_6, aug_6)
```

``` r
rmarkdown::render("C:/Users/peach/Documents/ST558/ST558_repos/vignette_project/_Rmd/README.Rmd", output_format = "github_document", 
                  output_dir = "C:/Users/peach/documents/ST558/ST558_repos/vignette_project/_posts", 
                  output_options = list(html_preview = FALSE)
                  )
```

## Welcome to GitHub Pages

You can use the [editor on
GitHub](https://github.com/peachgal/vignette_project/edit/main/README.md)
to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run
[Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from
the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your
writing. It includes conventions for

``` markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored
Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you
have selected in your [repository
settings](https://github.com/peachgal/vignette_project/settings/pages).
The name of this theme is saved in the Jekyll `_config.yml`
configuration file.

### Support or Contact

Having trouble with Pages? Check out our
[documentation](https://docs.github.com/categories/github-pages-basics/)
or [contact support](https://support.github.com/contact) and we’ll help
you sort it out.

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:
