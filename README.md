ST558 - Vignette Project - Covid-19 Data
================
Jasmine Wang
10/05/2021

## Getting an API

``` r
library(httr)
library(jsonlite)
```

    ## Warning: package 'jsonlite' was built under R version 4.0.5

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.5

    ## -- Attaching packages ------------------------------------------------------------------------------------------------ tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.5

    ## -- Conflicts --------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter()  masks stats::filter()
    ## x purrr::flatten() masks jsonlite::flatten()
    ## x dplyr::lag()     masks stats::lag()

``` r
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
#sep_1 <- GET("https://api.covid19api.com/country/united-states?from=2021-01-31T00:00:00Z&to=2021-01-31T23#:59:59Z")
dec_2 <- rawToChar(dec_1$content)
jan_2 <- rawToChar(jan_1$content)
feb_2 <- rawToChar(feb_1$content)
mar_2 <- rawToChar(mar_1$content)
apr_2 <- rawToChar(apr_1$content)
may_2 <- rawToChar(may_1$content)
jun_2 <- rawToChar(jun_1$content)
jul_2 <- rawToChar(jul_1$content)
aug_2 <- rawToChar(aug_1$content)

dec_3 <- fromJSON(dec_2)
jan_3 <- fromJSON(jan_2)
feb_3 <- fromJSON(feb_2)
mar_3 <- fromJSON(mar_2)
apr_3 <- fromJSON(apr_2)
may_3 <- fromJSON(may_2)
jun_3 <- fromJSON(jun_2)
jul_3 <- fromJSON(jul_2)
aug_3 <- fromJSON(aug_2)

dec_4 <- dec_3 %>% select(Province, Deaths, Active, Date) 
jan_4 <- jan_3 %>% select(Province, Deaths, Active, Date)
feb_4 <- feb_3 %>% select(Province, Deaths, Active, Date)
mar_4 <- mar_3 %>% select(Province, Deaths, Active, Date)
apr_4 <- apr_3 %>% select(Province, Deaths, Active, Date)
may_4 <- may_3 %>% select(Province, Deaths, Active, Date)
jun_4 <- jun_3 %>% select(Province, Deaths, Active, Date)
jul_4 <- jul_3 %>% select(Province, Deaths, Active, Date)
aug_4 <- aug_3 %>% select(Province, Deaths, Active, Date)
# sep_4 <- sep_3 %>% select(Province, Deaths, Active, Date)

dec_5 <- dec_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Dec")
jan_5 <- jan_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
feb_5 <- feb_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
mar_5 <- mar_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
apr_5 <- apr_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
may_5 <- may_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
jun_5 <- jun_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
jul_5 <- jul_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")
aug_5 <- aug_4 %>% group_by(Province) %>% summarise(across(c(Deaths, Active), sum)) %>% mutate(Month = "Jan")

#rbind(dec_5, jan_5)

#Deaths <- jan_5$Deaths - dec_5$Deaths
#Active <- jan_5$Active - dec_5$Active

jan_6 <- data.frame(jan_5$Province, dec_5$Deaths - dec_5$Deaths, jan_5$Active - dec_5$Active, "Jan")
feb_6 <- data.frame(feb_5$Province, jan_5$Deaths - dec_5$Deaths, feb_5$Active - jan_5$Active, "Feb")
mar_6 <- data.frame(mar_5$Province, feb_5$Deaths - dec_5$Deaths, mar_5$Active - feb_5$Active, "Mar")
apr_6 <- data.frame(apr_5$Province, mar_5$Deaths - dec_5$Deaths, apr_5$Active - mar_5$Active, "Apr")
may_6 <- data.frame(may_5$Province, apr_5$Deaths - dec_5$Deaths, may_5$Active - apr_5$Active, "May")
jun_6 <- data.frame(jun_5$Province, may_5$Deaths - dec_5$Deaths, jun_5$Active - may_5$Active, "Jun")
jul_6 <- data.frame(jul_5$Province, jun_5$Deaths - dec_5$Deaths, jul_5$Active - jun_5$Active, "Jul")
aug_6 <- data.frame(aug_5$Province, jul_5$Deaths - dec_5$Deaths, aug_5$Active - jul_5$Active, "Aug")

colnames(jan_6) <- c("State", "Deaths", "Cases", "Month")
colnames(feb_6) <- c("State", "Deaths", "Cases", "Month")
colnames(mar_6) <- c("State", "Deaths", "Cases", "Month")
colnames(apr_6) <- c("State", "Deaths", "Cases", "Month")
colnames(may_6) <- c("State", "Deaths", "Cases", "Month")
colnames(jun_6) <- c("State", "Deaths", "Cases", "Month")
colnames(jul_6) <- c("State", "Deaths", "Cases", "Month")
colnames(aug_6) <- c("State", "Deaths", "Cases", "Month")
rbind(jan_6, feb_6, mar_6, apr_6, may_6, jun_6, jul_6, aug_6)
```

    ##                        State Deaths   Cases Month
    ## 1                                 0 6051888   Jan
    ## 2                    Alabama      0   95552   Jan
    ## 3                     Alaska      0    7280   Jan
    ## 4             American Samoa      0       0   Jan
    ## 5                    Arizona      0  233941   Jan
    ## 6                   Arkansas      0   68938   Jan
    ## 7                 California      0  983631   Jan
    ## 8                   Colorado      0   61262   Jan
    ## 9                Connecticut      0   63264   Jan
    ## 10                  Delaware      0   20371   Jan
    ## 11          Diamond Princess      0       0   Jan
    ## 12      District of Columbia      0    7762   Jan
    ## 13                   Florida      0  393256   Jan
    ## 14                   Georgia      0  239729   Jan
    ## 15            Grand Princess      0       0   Jan
    ## 16                      Guam      0     263   Jan
    ## 17                    Hawaii      0    4467   Jan
    ## 18                     Idaho      0   21317   Jan
    ## 19                  Illinois      0  160196   Jan
    ## 20                   Indiana      0  113210   Jan
    ## 21                      Iowa      0   36686   Jan
    ## 22                    Kansas      0   50639   Jan
    ## 23                  Kentucky      0   96498   Jan
    ## 24                 Louisiana      0   83980   Jan
    ## 25                     Maine      0   14880   Jan
    ## 26                  Maryland      0   76565   Jan
    ## 27             Massachusetts      0  146693   Jan
    ## 28                  Michigan      0   76286   Jan
    ## 29                 Minnesota      0   45619   Jan
    ## 30               Mississippi      0   57932   Jan
    ## 31                  Missouri      0   90644   Jan
    ## 32                   Montana      0   12122   Jan
    ## 33                  Nebraska      0   23646   Jan
    ## 34                    Nevada      0   52429   Jan
    ## 35             New Hampshire      0   20769   Jan
    ## 36                New Jersey      0  165360   Jan
    ## 37                New Mexico      0   30394   Jan
    ## 38                  New York      0  435482   Jan
    ## 39            North Carolina      0  215394   Jan
    ## 40              North Dakota      0    5004   Jan
    ## 41  Northern Mariana Islands      0      10   Jan
    ## 42                      Ohio      0  191735   Jan
    ## 43                  Oklahoma      0   97930   Jan
    ## 44                    Oregon      0   28027   Jan
    ## 45              Pennsylvania      0  196438   Jan
    ## 46               Puerto Rico      0   17007   Jan
    ## 47              Rhode Island      0   26078   Jan
    ## 48            South Carolina      0  134133   Jan
    ## 49              South Dakota      0    8796   Jan
    ## 50                 Tennessee      0  138316   Jan
    ## 51                     Texas      0  594552   Jan
    ## 52                      Utah      0   69616   Jan
    ## 53                   Vermont      0    4515   Jan
    ## 54            Virgin Islands      0     366   Jan
    ## 55                  Virginia      0  153763   Jan
    ## 56                Washington      0   64021   Jan
    ## 57             West Virginia      0   34981   Jan
    ## 58                 Wisconsin      0   70510   Jan
    ## 59                   Wyoming      0    7345   Jan
    ## 60                            97194 2336398   Feb
    ## 61                   Alabama   2861   31372   Feb
    ## 62                    Alaska     56    3918   Feb
    ## 63            American Samoa      0       0   Feb
    ## 64                   Arizona   4256   55518   Feb
    ## 65                  Arkansas   1192   26772   Feb
    ## 66                California  14994  231708   Feb
    ## 67                  Colorado    826   31807   Feb
    ## 68               Connecticut   1051   29347   Feb
    ## 69                  Delaware    244    8611   Feb
    ## 70          Diamond Princess      0       0   Feb
    ## 71      District of Columbia    127    3622   Feb
    ## 72                   Florida   4806  183471   Feb
    ## 73                   Georgia   3264   93979   Feb
    ## 74            Grand Princess      0       0   Feb
    ## 75                      Guam      8     155   Feb
    ## 76                    Hawaii    122    1755   Feb
    ## 77                     Idaho    289    8322   Feb
    ## 78                  Illinois   3275   59147   Feb
    ## 79                   Indiana   1906   33954   Feb
    ## 80                      Iowa   1010   16487   Feb
    ## 81                    Kansas   1049   18115   Feb
    ## 82                  Kentucky   1122   40849   Feb
    ## 83                 Louisiana   1371   28725   Feb
    ## 84                     Maine    243    5198   Feb
    ## 85                  Maryland   1232   26894   Feb
    ## 86             Massachusetts   2154   55582   Feb
    ## 87                  Michigan   2489   33320   Feb
    ## 88                 Minnesota    886   22504   Feb
    ## 89               Mississippi   1258   19158   Feb
    ## 90                  Missouri   1347   28963   Feb
    ## 91                   Montana    272    5881   Feb
    ## 92                  Nebraska    269   10007   Feb
    ## 93                    Nevada   1148   14759   Feb
    ## 94             New Hampshire    298    9362   Feb
    ## 95                New Jersey   2442   90759   Feb
    ## 96                New Mexico    806   10635   Feb
    ## 97                  New York   5651  219939   Feb
    ## 98            North Carolina   2587   99145   Feb
    ## 99              North Dakota    148    2157   Feb
    ## 100 Northern Mariana Islands      0      11   Feb
    ## 101                     Ohio   3702   70278   Feb
    ## 102                 Oklahoma   1058   34479   Feb
    ## 103                   Oregon    480   12930   Feb
    ## 104             Pennsylvania   5626   85198   Feb
    ## 105              Puerto Rico    326    6466   Feb
    ## 106             Rhode Island    411   11023   Feb
    ## 107           South Carolina   1746   71933   Feb
    ## 108             South Dakota    290    4067   Feb
    ## 109                Tennessee   2743   45382   Feb
    ## 110                    Texas   9008  270046   Feb
    ## 111                     Utah    396   24341   Feb
    ## 112                  Vermont     38    3203   Feb
    ## 113           Virgin Islands      1     247   Feb
    ## 114                 Virginia   1432   69183   Feb
    ## 115               Washington    824   27505   Feb
    ## 116            West Virginia    686   10578   Feb
    ## 117                Wisconsin   1192   24647   Feb
    ## 118                  Wyoming    158    2407   Feb
    ## 119                          162817 1776176   Mar
    ## 120                  Alabama   5102   21511   Mar
    ## 121                   Alaska     84    4582   Mar
    ## 122           American Samoa      0       0   Mar
    ## 123                  Arizona   7116   24042   Mar
    ## 124                 Arkansas   1567    7600   Mar
    ## 125               California  26615   94795   Mar
    ## 126                 Colorado   1137   33622   Mar
    ## 127              Connecticut   1627   30411   Mar
    ## 128                 Delaware    361    7866   Mar
    ## 129         Diamond Princess      0       0   Mar
    ## 130     District of Columbia    231    3868   Mar
    ## 131                  Florida   9179  145941   Mar
    ## 132                  Georgia   6361   51267   Mar
    ## 133           Grand Princess      0       0   Mar
    ## 134                     Guam     10      68   Mar
    ## 135                   Hawaii    151    2350   Mar
    ## 136                    Idaho    424    9294   Mar
    ## 137                 Illinois   4757   57101   Mar
    ## 138                  Indiana   3183   24333   Mar
    ## 139                     Iowa   1580   14665   Mar
    ## 140                   Kansas   1975    7765   Mar
    ## 141                 Kentucky   2014   20801   Mar
    ## 142                Louisiana   2120   14300   Mar
    ## 143                    Maine    356    5829   Mar
    ## 144                 Maryland   1974   28834   Mar
    ## 145            Massachusetts   3695   53365   Mar
    ## 146                 Michigan   3466   99075   Mar
    ## 147                Minnesota   1169   34560   Mar
    ## 148              Mississippi   1894   10000   Mar
    ## 149                 Missouri   2557   18188   Mar
    ## 150                  Montana    396    4518   Mar
    ## 151                 Nebraska    431    8366   Mar
    ## 152                   Nevada   1835    9716   Mar
    ## 153            New Hampshire    411    8684   Mar
    ## 154               New Jersey   4210  118151   Mar
    ## 155               New Mexico   1239    6299   Mar
    ## 156                 New York   9618  226598   Mar
    ## 157           North Carolina   4464   54684   Mar
    ## 158             North Dakota    176    3261   Mar
    ## 159 Northern Mariana Islands      0      16   Mar
    ## 160                     Ohio   5065   49551   Mar
    ## 161                 Oklahoma   1939   13658   Mar
    ## 162                   Oregon    731    9240   Mar
    ## 163             Pennsylvania   7965   93532   Mar
    ## 164              Puerto Rico    533    7091   Mar
    ## 165             Rhode Island    572   11590   Mar
    ## 166           South Carolina   3250   34207   Mar
    ## 167             South Dakota    400    5285   Mar
    ## 168                Tennessee   4504   36345   Mar
    ## 169                    Texas  15631  134342   Mar
    ## 170                     Utah    666   14219   Mar
    ## 171                  Vermont     68    4054   Mar
    ## 172           Virgin Islands      2     260   Mar
    ## 173                 Virginia   3520   41226   Mar
    ## 174               Washington   1495   24422   Mar
    ## 175            West Virginia    962    9507   Mar
    ## 176                Wisconsin   1772   18373   Mar
    ## 177                  Wyoming    233    1887   Mar
    ## 178                          200321 1861948   Apr
    ## 179                  Alabama   5727   12192   Apr
    ## 180                   Alaska    107    5213   Apr
    ## 181           American Samoa      0       0   Apr
    ## 182                  Arizona   8103   20329   Apr
    ## 183                 Arkansas   1950    5214   Apr
    ## 184               California  32988   70895   Apr
    ## 185                 Colorado   1293   48697   Apr
    ## 186              Connecticut   1894   28401   Apr
    ## 187                 Delaware    498    9537   Apr
    ## 188         Diamond Princess      0       0   Apr
    ## 189     District of Columbia    278    3143   Apr
    ## 190                  Florida  11752  174047   Apr
    ## 191                  Georgia   8121   39504   Apr
    ## 192           Grand Princess      0       0   Apr
    ## 193                     Guam     13     155   Apr
    ## 194                   Hawaii    175    3077   Apr
    ## 195                    Idaho    526    7036   Apr
    ## 196                 Illinois   5601   89797   Apr
    ## 197                  Indiana   3649   33640   Apr
    ## 198                     Iowa   1852   13235   Apr
    ## 199                   Kansas   2153    6585   Apr
    ## 200                 Kentucky   3467   16840   Apr
    ## 201                Louisiana   2653   13407   Apr
    ## 202                    Maine    396   10668   Apr
    ## 203                 Maryland   2391   35611   Apr
    ## 204            Massachusetts   4762   52968   Apr
    ## 205                 Michigan   4076  187896   Apr
    ## 206                Minnesota   1544   55972   Apr
    ## 207              Mississippi   2245    6587   Apr
    ## 208                 Missouri   3129    9672   Apr
    ## 209                  Montana    476    4131   Apr
    ## 210                 Nebraska    529   10416   Apr
    ## 211                   Nevada   2127   11461   Apr
    ## 212            New Hampshire    479   10645   Apr
    ## 213               New Jersey   5519   87414   Apr
    ## 214               New Mexico   1460    5951   Apr
    ## 215                 New York  12298  173076   Apr
    ## 216           North Carolina   5364   55081   Apr
    ## 217             North Dakota    199    4356   Apr
    ## 218 Northern Mariana Islands      0       9   Apr
    ## 219                     Ohio   5663   54217   Apr
    ## 220                 Oklahoma   2464    6730   Apr
    ## 221                   Oregon    906   19688   Apr
    ## 222             Pennsylvania   9059  124708   Apr
    ## 223              Puerto Rico    615   24294   Apr
    ## 224             Rhode Island    689   10805   Apr
    ## 225           South Carolina   3850   26678   Apr
    ## 226             South Dakota    447    4869   Apr
    ## 227                Tennessee   4997   35295   Apr
    ## 228                    Texas  20186  100051   Apr
    ## 229                     Utah    853   11602   Apr
    ## 230                  Vermont     91    3658   Apr
    ## 231           Virgin Islands      3     217   Apr
    ## 232                 Virginia   5220   40096   Apr
    ## 233               Washington   1786   38302   Apr
    ## 234            West Virginia   1338   11387   Apr
    ## 235                Wisconsin   2073   24174   Apr
    ## 236                  Wyoming    262    1825   Apr
    ## 237                          224111  898648   May
    ## 238                  Alabama   6069   15233   May
    ## 239                   Alaska    141    2038   May
    ## 240           American Samoa      0       0   May
    ## 241                  Arizona   8460   18649   May
    ## 242                 Arkansas   2063    5562   May
    ## 243               California  35451   46189   May
    ## 244                 Colorado   1481   31923   May
    ## 245              Connecticut   2102    7967   May
    ## 246                 Delaware    564    4425   May
    ## 247         Diamond Princess      0       0   May
    ## 248     District of Columbia    319    1174   May
    ## 249                  Florida  13488   85687   May
    ## 250                  Georgia   9256   23007   May
    ##  [ reached 'max' / getOption("max.print") -- omitted 222 rows ]

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
