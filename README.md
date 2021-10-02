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

``` r
rmarkdown::render("C:/Users/peach/Documents/ST558/ST558_repos/vignette_project/_Rmd/README.Rmd", 
                  output_format = "github_document", 
                  output_dir = "C:/Users/peach/documents/ST558/ST558_repos/vignette_project/_posts", 
                  output_options = list(html_preview = FALSE, toc = TRUE, df_print = "tibble")
                  )
```

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
summary3 <- mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_total = round(mean(Total)), sd_total = round(sd(Total)), median_total = round(median(Total)), IQR_total = round(IQR(Total)))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
summary4 <- mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_death = mean(Death), sd_death = sd(Death), median_death = median(Death), IQR_death = IQR(Death))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
library(tidyverse)
library(ggplot2)

barplot1 <- summary3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina"))
g <- ggplot(data = barplot1, aes(x = Province, y = avg_total, fill = vaccine))
g + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Average Total Cases", title = "Figure 1. Average total cases at each timeline for each state") + 
  scale_fill_discrete(name = "Vaccine timeline") + 
  theme(axis.text.x = element_text(angle = 45, size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-1.png)<!-- -->

``` r
barplot2 <- summary4 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina"))
d <- ggplot(data = barplot2, aes(x = Province, y = avg_death, fill = vaccine))
d + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Average Total Cases", title = "Figure 2. Average total deaths at each timeline for each state") + 
  scale_fill_discrete(name = "Vaccine timeline") + 
  theme(axis.text.x = element_text(angle = 45, size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-2.png)<!-- -->

``` r
# mydata3

boxplot1 <- ggplot(data = mydata3, aes(x = vaccine, y = Total))
boxplot1 + geom_boxplot(fill = "white", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0, 300000)) + 
  geom_jitter(aes(color = vaccine), size = 3) + 
  labs(title = "Figure 3. Boxplot for total number of cases") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-3.png)<!-- -->

``` r
boxplot2 <- ggplot(data = mydata3, aes(x = vaccine, y = Death))
boxplot2 + geom_boxplot(fill = "white", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0, 10000)) + 
  geom_jitter(aes(color = vaccine), size = 3) + 
  labs(title = "Figure 4. Boxplot for total number of deaths") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-4.png)<!-- -->

``` r
scatter <- ggplot(data = mydata3, aes(x = Cases, y = Death, color = vaccine))
scatter + geom_point(aes(shape = Total_cases), size = 3) + 
  coord_cartesian(xlim=c(0, 1000000), ylim=c(0, 20000)) +
  geom_smooth(method = lm, lwd = 2) + 
  labs(title = "Figure 5. Scappterplot: Number of cases vs Deaths") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-5.png)<!-- -->

``` r
his <- ggplot(data = mydata3, aes(x = Total, color = vaccine))
his + geom_histogram(binwidth = 10) + 
  coord_cartesian(xlim=c(0, 1000000)) + 
  labs(title = "Figure 6. Histogram of Total Cases via Timelines") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-6.png)<!-- -->

``` r
mydata3$Month <- ordered(mydata3$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))

neighbor <- mydata3 %>% filter(State %in% c("North Carolina", "South Carolina", "Tennessee","Georgia", "Kentucky", "Virginia", "West Virginia"))
lineplot1 <- ggplot(data = neighbor, aes(x = Month, y = Total, color = State))
lineplot1 + geom_line(aes(group = State)) + geom_point() + 
  labs(title = "Figure 7. Line plot of Total Cases of NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-7.png)<!-- -->

``` r
lineplot1 <- ggplot(data = neighbor, aes(x = Month, y = Death, color = State))
lineplot1 + geom_line(aes(group = State)) + geom_point() + 
  labs(title = "Figure 8. Line plot of Total Deaths of NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
```

![](../images/unnamed-chunk-3-8.png)<!-- -->

``` r
#d <- ggplot(data = crabs, aes(x = spine))
#d + geom_bar(aes(fill = as.factor(y)), position = "dodge") + 
#  labs(x = "Female Crab Spine Condition", 
#       title = "Figure 2. Female Crab Spine Condition vs. Satellite Count") + 
#  scale_fill_discrete(name = NULL) + 
#  theme(axis.text.x = element_text(size = 10), 
#        axis.text.y = element_text(size = 10), 
#        axis.title.x = element_text(size = 15), 
#        axis.title.y = element_text(size = 15), 
#        legend.key.size = unit(1, 'cm'), 
#        legend.text = element_text(size = 13), 
#        title = element_text(size = 12))


#p <- ggplot(data = crabs, aes(x = spine))
#p + geom_bar(aes(fill = as.factor(color)), position = "dodge") + 
#  labs(x = "Female Crab Spine Condition", 
#       title = "Figure. 3 Female Crab Spine Condition vs. Female Crab Color") + 
#  scale_fill_discrete(name = "Female Crab Color") + 
#  theme(axis.text.x = element_text(size = 10), 
#        axis.text.y = element_text(size = 10), 
#        axis.title.x = element_text(size = 15), 
#        axis.title.y = element_text(size = 15), 
#        legend.key.size = unit(1, 'cm'), 
#        legend.title = element_text(size = 15), 
#        legend.text = element_text(size = 13), 
#        title = element_text(size = 12))
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
