ST558 - Vignette Project - Covid-19 Data
================
Jasmine Wang
10/05/2021

-   [Writing Functions](#writing-functions)
-   [Getting an API](#getting-an-api)
-   [Welcome to GitHub Pages](#welcome-to-github-pages)
    -   [Markdown](#markdown)
    -   [Jekyll Themes](#jekyll-themes)
    -   [Support or Contact](#support-or-contact)
-   [R Markdown](#r-markdown)

``` r
knitr::opts_chunk$set(fig.path = "../images/")
```

``` r
rmarkdown::render("C:/Users/peach/Documents/ST558/ST558_repos/vignette_project/_Rmd/ST558_vignette_proj.Rmd", 
                  output_format = "github_document", 
                  output_file = "C:/Users/peach/documents/ST558/ST558_repos/vignette_project/_README/README.md", 
                  output_options = list(html_preview = FALSE, toc = TRUE, df_print = "tibble")
)
```

## Writing Functions

``` r
library(httr)
library(jsonlite)
library(tidyverse)







get_api <- function(name, country, status, vintage_from, vintage_to, time_from = "00:00:00", time_to = "00:00:00"){
  
  base_url <- "https://api.covid19api.com"
  
  if(!is.null(name)) {
    
    api_url <- paste(base_url, tolower(name), sep = "/")
  }
  else if(!is.null(country)) {
    
    country_name <- if_else(grepl(" ", country, ignore.case = TRUE), tolower(sub(" ", "-", country)), tolower(country))
    
    api_url <- paste(base_url, "country", country_name, sep = "/")
    
    if(!is.null(status)) {
      
      status_name <- tolower(status)
      
      if(!is.nul(vintage_from) & !is.null(vintage_to)) {
        
        x <- parse_date_time("05/25/2017", orders = "mdy")
        x <- as.Date(x)
        print(x)
        
        
        
        date_n_time <- paste0("?", "from=", vintage_from, "T", time_from, "Z&to=", vintage_to, "T", time_to, "Z")
        
        
      }
      api_url <- paste0(base_url, "status", "/", status_name, "?")
      
      
    }
  }
  
  #status = comfirmed, recovered deaths
  
  
  api_url
  
  #something1 <- GET()
  #something2 <- rawToChar(something1$content)
  #something3 <- fromJSON(something2)
  #return(something3)
}



####### Country function ##################################################
country <- function(nation, ...){
  
  if(nchar(nation) > 3) {
    
    
    nation_name <- if_else(grepl(" ", nation, ignore.case = TRUE), tolower(sub(" ", "-", nation)), tolower(nation))
    nation_name
    
  } else {
    
    nation_name <- countrycode(as.character(nation), origin = "iso3c", destination = "country.name")
    nation_name <- if_else(grepl(" ", nation_name, ignore.case = TRUE), tolower(sub(" ", "-", nation_name)), tolower(nation_name))
    nation_name
  }
}

##### Date function ########################################################
date_1 <- function(date){
  
  y <- c(date) #, "04-16-2016")
  y <- as_tibble(y)
  
  if(grepl("/", y$value, ignore.case = TRUE)){
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="/")
    
  } else {
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="-")
    
  }
  if(nchar(temp$first) == 4){
    
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE)
    temp$date
    
  } else {
    
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE)
    temp$date <- mdy(temp$date)
    temp$date  
  }
}
# get_api(name = "summary")
# get_api(country = "UniteD States")
```

## Getting an API

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)

# url <- "https://api.covid19api.com/country/united-states?from=2020-03-01T00:00:00Z&to=2020-04-01T00:00:00Z"
dec_1 <- GET("https://api.covid19api.com/country/united-states?from=2020-12-31T00:00:00Z&to=2020-12-31T23:59:59Z")
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
data_11
data_22 <- data_2 %>% group_by(Province, Date) %>% summarise(across(c(Deaths, Active), sum))
names(data_22) <- c("State", "Date2", "Deaths2", "Active2")
mydata1 <- cbind(data_11, data_22)
mydata1
mydata2 <- mydata1 %>% mutate(Death = Deaths - Deaths2, Cases = Active - Active2, Total = Death + Cases, 
                              Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>% 
  select(State, Month, Death, Cases, Total)

write_csv(x = mydata2, path = "../_Data/covid_data.csv")
```

``` r
`%!in%` <- Negate(`%in%`)

mydata3 <- mydata2 %>% filter(Province %!in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands")) %>% 
  mutate(f500_deaths = if_else(Death < 500, "Less than 500 deaths", "More than 500 deaths"), 
         Total_cases = if_else(Total < 9000, "1. Less than 9000", 
                             if_else(Total < 30000, "2. Between 9000 and 30,000", "3. More than 30,000")), 
         vaccine = if_else(Month %in% c("Jan", "Feb", "Mar", "Apr"), "1. Vaccine-ing", 
                           if_else(Month %in% c("May", "Jun", "Jul"), "2. Some vaccined", "3. Back to school")))
dim(mydata3)
names(mydata3)
# 1st contingency table
table(mydata3$Total_cases, mydata3$vaccine)

# 2nd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Death) %>%
  pivot_wider(names_from = "Month", values_from = "Death") %>% arrange(desc(Jan))
# 3rd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Total) %>%
  pivot_wider(names_from = "Month", values_from = "Total") %>% arrange(desc(Jan))

# summaries
summary3 <- mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_total = round(mean(Total)), sd_total = round(sd(Total)), median_total = round(median(Total)), IQR_total = round(IQR(Total)))

summary4 <- mydata3 %>% group_by(Province, vaccine) %>% 
  summarise(avg_death = mean(Death), sd_death = sd(Death), median_death = median(Death), IQR_death = IQR(Death))
```

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

his <- ggplot(data = mydata3, aes(x = Total))
his + geom_histogram() + 
  coord_cartesian(xlim=c(0, 1000000)) + 
  labs(title = "Figure 6. Histogram of Total Cases via Timelines") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))

# mydata3$Month <- ordered(mydata3$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
mydata3$Month <- factor(mydata3$Month, levels = unique(mydata3$Month))

neighbor <- mydata3 %>% filter(State %in% c("North Carolina", "South Carolina", "Tennessee","Georgia", "Kentucky", "Virginia", "West Virginia"))
lineplot1 <- ggplot(data = neighbor, aes(x = Month, y = Total, color = State))
lineplot1 + geom_line(aes(group = State)) + geom_point() + 
  labs(title = "Figure 7. Line plot: Total cases of NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))

lineplot1 <- ggplot(data = neighbor, aes(x = Month, y = Death, color = State))
lineplot1 + geom_line(aes(group = State)) + geom_point() + 
  labs(title = "Figure 8. Line plot: Total deaths of NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 15))
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
