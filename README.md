ST558 - Vignette Project - Covid-19 Data
================
Jasmine Wang
10/07/2021

-   [1. Goal of Vignette](#1-goal-of-vignette)
    -   [1.1. Required Packages](#11-required-packages)
-   [2. Customized Functions](#2-customized-functions)
    -   [2.1. A List of Customized
        Functions](#21-a-list-of-customized-functions)
    -   [2.2. A List of Input Arguments and their
        Defaults](#22-a-list-of-input-arguments-and-their-defaults)
    -   [2.3 Using an API](#23-using-an-api)
-   [3. Getting the Data](#3-getting-the-data)
    -   [3.1. Combining Date Sets](#31-combining-date-sets)
-   [4. Data Manipulation](#4-data-manipulation)
    -   [4.1. Creating Numeric
        Variables](#41-creating-numeric-variables)
    -   [4.2. Creating Categorical
        Variables](#42-creating-categorical-variables)
-   [5. Exploratory Data Analysis](#5-exploratory-data-analysis)
    -   [5.1. Contingency Tables](#51-contingency-tables)
    -   [5.2. Numerical Summaries](#52-numerical-summaries)
    -   [5.3. Visualization](#53-visualization)
-   [6. Conclusion](#6-conclusion)

``` r
knitr::opts_chunk$set(fig.path = "/images/")
```

``` r
rmarkdown::render("C:/Users/peach/Documents/ST558/ST558_repos/vignette_project/_Rmd/ST558_vignette_proj.Rmd", 
                  output_format = "github_document", 
                  output_file = "C:/Users/peach/documents/ST558/ST558_repos/vignette_project/README.md", 
                  output_options = list(html_preview = FALSE, toc = TRUE, toc_depth = 3, df_print = "tibble")
)
```

## 1. Goal of Vignette

Since Covid-19 wide-spread the whole globe, most of people’s lives have
been affected and even changed. However, with the administration of
vaccination, we all hope to get back to the life it was like before. To
do this, we need to get the majority population vaccination rate up to
more than 70% for the community immunity to take effect. The purpose of
this vignette is to see how the vaccination and state-mandated
regulations have affected the active cases and deaths across the United
States. Therefore, we are going to sum up all the active cases and
deaths for each state and each month. Then, we will look at nite months
of state-wide data about the active cases and deaths starting from
January 2021.

We will divide the months into three different timelines:

-   *Vaccinat-ING*: January, February, March, April  
-   *Some vaccinated*: May, June, July  
-   *Back to school*: August, September

During each timeline, we can see how the number of active cases and
deaths are affected by these events.

The criteria I am using is based on vaccination progress in North
Carolina. Since all groups are open to vaccination on April 7th, 2021,
and it takes roughly four weeks to develop full immunity, I categorized
the timeline before May to be on-going vaccination period while the
antibodies against the virus are still developing. The months between
May and July are when approximately 40% of the general population are
vaccinated with at least one shot. Then, the students are going back to
school in person in August and September. We will see how during these
different timelines are the active cases and deaths changed in the
exploratory data analysis section.

Therefore, the goal of this vignette is to create functions to help
users query Covid-19 related data from [Covid-19 API
site](https://covid19api.com) and parse the data into a nicely formatted
tibble or data frame. Upon obtaining the data, we can then use it to
perform some exploratory data analysis such as numerical summaries and
plots for better visualization.

### 1.1. Required Packages

Below is a list of packages needed to create the vignette:

-   `knitr`  
-   `httr`  
-   `jsonlite`  
-   `countrycode`  
-   `tidyverse`  
-   `ggplot2`

``` r
library(knitr)
library(httr)
library(jsonlite)
library(tidyverse)
library(countrycode)

######## General summary/countries/all####################################################
general_input <- function(gen_input, ...){
  
  input1 <- tolower(gen_input, ...) 
  option <- c("all", "summary", "countries")
  num <- charmatch(input1, option, ...)
  
  if(!is.na(num)) {
    
    status_string <- option[num]
    status_string
    
  } else {
    
    print("Sorry mate, your input is not valid! Please choose another one.")
  }
}

####### Country ###########################################################################
country <- function(country_name, ...){
  
  if(nchar(country_name) == 3) {
    
    nation_name <- countrycode(as.character(country_name), origin = "iso3c", destination = "country.name", ...)
    nation_name <- if_else(grepl(" ", nation_name, ignore.case = TRUE), tolower(sub(" ", "-", nation_name)), tolower(nation_name), ...)
    nation_name <- paste("country", nation_name, sep = "/", ...)
    nation_name
    
  } else if(nchar(country_name) == 2) {
    
    nation_name <- countrycode(as.character(country_name), origin = "iso2c", destination = "country.name", ...)
    nation_name <- if_else(grepl(" ", nation_name, ignore.case = TRUE), tolower(sub(" ", "-", nation_name)), tolower(nation_name), ...)
    nation_name <- paste("country", nation_name, sep = "/", ...)
    nation_name
    
  } else {
    
    nation_name <- if_else(grepl(" ", country_name, ignore.case = TRUE), tolower(sub(" ", "-", country_name)), tolower(country_name), ...)
    nation_name <- paste("country", nation_name, sep = "/", ...)
    nation_name
  }
}
#countrycode('Albania', origin = 'country.name', destination = 'iso3c')
#countrycode(nation, origin = "iso3c", destination = "country.name")

####### Status ### confirmed/recovered/deaths #####################################
status_input <- function(status, ...){
  
  input <- tolower(status, ...)
  option <- c("confirmed", "recovered", "deaths")
  num <- charmatch(input, option, ...)
  
  if(!is.na(num)) {
    
    status_string <- paste("status", option[num], sep="/", ...)
    status_string
    
  } else {
    
    print("Sorry mate, your input is not valid! Please choose another one.")
  }
}

####### Date Function ####################################################################
date_input_from <- function(date_from, ...){
  
  y <- c(date_from)
  y <- as_tibble(y)
  
  if(grepl("/", y$value, ignore.case = TRUE, ...)){
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="/", ...)
    
  } else if(grepl("-", y$value, ignore.case = TRUE, ...)) {
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="-", ...)
    
  } else {
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep=" ", ...)
  }
  if(nchar(temp$first) == 4){
    
    if(nchar(temp$second) == 1){
      
      temp$second <- paste0("0", temp$second, ...)
      
      if(nchar(temp$third) == 1){
        
        temp$third <- paste0("0", temp$third, ...)
      }
    }
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE, ...)
    temp$date
    
  } else {
    
    if(nchar(temp$second) == 1){
      
      temp$second <- paste0("0", temp$second, ...)
      
      if(nchar(temp$third) == 1){
        
        temp$third <- paste0("0", temp$third, ...)
      }
    }
    
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE, ...)
    temp$date <- mdy(temp$date, ...)
    temp$date  
  }
}

date_input_to <- function(date_to, ...){
  
  y <- c(date_to)
  y <- as_tibble(y)
  
  if(grepl("/", y$value, ignore.case = TRUE, ...)){
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="/", ...)
    
  } else if(grepl("-", y$value, ignore.case = TRUE, ...)) {
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep="-", ...)
    
  } else {
    
    temp <- y %>% separate(value, c("first", "second", "third"), sep=" ", ...)
  }
  if(nchar(temp$first) == 4){
    
    if(nchar(temp$second) == 1){
      
      temp$second <- paste0("0", temp$second, ...)
      
      if(nchar(temp$third) == 1){
        
        temp$third <- paste0("0", temp$third, ...)
      }
    }
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE, ...)
    temp$date
    
  } else {
    
    if(nchar(temp$second) == 1){
      
      temp$second <- paste0("0", temp$second, ...)
      
      if(nchar(temp$third) == 1){
        
        temp$third <- paste0("0", temp$third, ...)
      }
    }
    
    temp <- unite(temp, date, first, second, third, sep="-", remove = TRUE, ...)
    temp$date <- mdy(temp$date, ...)
    temp$date  
  }
}

######### Time Function ##################################################################
time_input_from <- function(time_from, ...){
  
  times <- c(time_from)
  times <- as_tibble(times)
  
  if(grepl(":", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep=":", ...)
    
  } else if(grepl("-", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep="-", ...)
    
  } else if(grepl("/", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep="/", ...)
    
  }
  
  if(nchar(times$first) == 1){
    
    times$first <- paste0("0", times$first, ...)
    
    if(nchar(times$second) == 1){
      
      times$second <- paste0("0", times$second, ...)
      
      if(nchar(times$third) == 1){
        
        times$third <- paste0("0", times$third, ...)
        
      }
    }
  }
  
  if(as.numeric(times$first) %in% c(0:23)){
    
    if(as.numeric(times$second) %in% c(0:59)){
      
      if(as.numeric(times$third) %in% c(0:59)){
        
        
        times <- unite(times, value, first, second, third, sep=":", remove = TRUE, ...)
        times$value
        
      } else {
        
        print("Sorry mate, your input is not valid! Please choose a time between 0 and 59 seconds.")
        
      }
    } else {
      
      print("Sorry mate, your input is not valid! Please choose a time between 0 and 59 minutes.")
      
    }
  } else {
    
    print("Sorry mate, your input is not valid! Please choose a military time between 0 and 23 hours.")
    
  }
}

time_input_to <- function(time_to, ...){
  
  times <- c(time_to)
  times <- as_tibble(times)
  
  if(grepl(":", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep=":", ...)
    
  } else if(grepl("-", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep="-", ...)
    
  } else if(grepl("/", times$value, ignore.case = TRUE, ...)) {
    
    times <- times %>% separate(value, c("first", "second", "third"), sep="/", ...)
    
  }
  
  if(nchar(times$first) == 1){
    
    times$first <- paste0("0", times$first, ...)
    
    if(nchar(times$second) == 1){
      
      times$second <- paste0("0", times$second, ...)
      
      if(nchar(times$third) == 1){
        
        times$third <- paste0("0", times$third, ...)
        
      }
    }
  }
  
  if(as.numeric(times$first) %in% c(0:23)){
    
    if(as.numeric(times$second) %in% c(0:59)){
      
      if(as.numeric(times$third) %in% c(0:59)){
        
        
        times <- unite(times, value, first, second, third, sep=":", remove = TRUE, ...)
        times$value
        
      } else {
        
        print("Sorry mate, your input is not valid! Please choose a time between 0 and 59 seconds.")
        
      }
    } else {
      
      print("Sorry mate, your input is not valid! Please choose a time between 0 and 59 minutes.")
      
    }
  } else {
    
    print("Sorry mate, your input is not valid! Please choose a military time between 0 and 23 hours.")
    
  }
}

############# A P I ##########################################################################
ref_general <- function(gen_input = "countries", ...){
  
  info1 <- general_input(gen_input, ...)
  
  api_url <- if_else(info1 %in% c("all", "summary", "countries"), 
                     paste("https://api.covid19api.com", info1, sep = "/"), info1, ...)
  api_url
  
}

dayone_all_status <- function(country_name = "BB", ...){
  
  info2 <- country(country_name, ...)
  
  api_url <- paste("https://api.covid19api.com/dayone", info2, sep = "/", ...)
  api_url
  
}

dayone_status <- function(country_name = "BB", status = "confirmed", ...){
  
  info2 <- country(country_name, ...)
  info3 <- status_input(status, ...)
  
  api_url <- paste("https://api.covid19api.com/dayone", info2, info3, sep = "/", ...)
  api_url
  
}

dayone_status_live <- function(country_name = "BB", status = "confirmed", ...){
  
  info2 <- country(country_name, ...)
  info3 <- status_input(status, ...)
  
  api_url <- paste("https://api.covid19api.com/dayone", info2, info3, "live", sep = "/", ...)
  api_url
  
}

datetime_specific_all_status <- function(country_name="BB", date_from="2020-05-01", date_to="2020-05-03", 
                                             time_from="00:00:00", time_to="00:00:00", ...){
  
  info2 <- country(country_name, ...)
  
  date1 <- date_input_from(date_from, ...)
  date2 <- date_input_to(date_to, ...)
  time1 <- time_input_from(time_from, ...)
  time2 <- time_input_to(time_to, ...)
  
  latter <- paste0("?from=", date1, "T", time1, "Z&to=", date2, "T", time2, "Z", ...)
  api_url <- paste0("https://api.covid19api.com/", info2, latter, ...)
  api_url
  
}

datetime_specific_status <- function(country_name="BB", status="confirmed", date_from="2020-05-01", date_to="2020-05-03", 
                                         time_from="00:00:00", time_to="00:00:00", ...){
  
  info2 <- country(country_name, ...)
  info3 <- status_input(status, ...)
  
  date1 <- date_input_from(date_from, ...)
  date2 <- date_input_to(date_to, ...)
  time1 <- time_input_from(time_from, ...)
  time2 <- time_input_to(time_to, ...)
  
  latter <- paste0("?from=", date1, "T", time1, "Z&to=", date2, "T", time2, "Z", ...)
  api_url <- paste0("https://api.covid19api.com/", info2, "/", info3, latter, ...)
  api_url
  
}

live_all_status <- function(country_name = "BB", ...){
  
    info2 <- country(country_name, ...)
  
  api_url <- paste("https://api.covid19api.com/live", info2, sep = "/", ...)
  api_url
  
}

live_status <- function(country_name = "BB", status = "confirmed", ...){
  
  info2 <- country(country_name, ...)
  info3 <- status_input(status, ...)
  
  api_url <- paste("https://api.covid19api.com/live", info2, info3, sep = "/", ...)
  api_url
  
}

live_status_after_date <- function(country_name = "BB", status = "confirmed", 
                                   date_from="2020-05-01", time_from="00:00:00", ...){
  
  info2 <- country(country_name, ...)
  info3 <- status_input(status, ...)
  
  date1 <- date_input_from(date_from, ...)
  time1 <- time_input_from(time_from, ...)
  datetime <- paste0(date_from, "T", time_from, "Z", ...)
  
  api_url <- paste("https://api.covid19api.com/live", info2, info3, "date", datetime,  sep = "/", ...)
  api_url
  
}
```

## 2. Customized Functions

I created some customized functions implemented with some user-friendly
input arguments to query the Covid-19 data from [Covid-19
API](https://covid19api.com/). Then, I created a wrapper function,
`choose_api`, which contains those nine customized functions to obtain
the correct API URL to query the data needed for the users. Users would
only need to supply with arguments like the country they want to query
from, status of the query (total confirmed cases, recovered cases, or
number of deaths), and starting from certain date and time they want to
query the data. Users can use `choose_api` function to check if their
query API URL is correct to query the data they need. Then, they can use
the exact same arguments they used in the `choose_api` function in the
`get_data` function to obtain the parsed data in a usable form. Thus, if
the API URL obtained from `choose_api` function is not correct, those
same arguments are surely not going to work in the `get_data` function.
Therefore, `choose_api` function is a way to make sure if the
user-supplied arguments are correct, and the obtained API URL is what
the users want to query.

### 2.1. A List of Customized Functions

Let’s look at the options of the data the customized functions can
query:

1.  **general** option, `ref_general` function:
    -   *summary* returns a summary of new and total cases per country,
        updated daily.  
    -   *countries* returns all the available countries and provinces,
        and the country slug for per country requests.  
    -   *all* returns all daily data. WARNING: This call results in &gt;
        10MB of data being returned and should be used infrequently.  
    -   `choose_api(type = 1, gen_input = "summary")`
    -   <https://api.covid19api.com/summary>
2.  **option1** API, `dayone_all_status`:
    -   Returns all cases types including confirmed cases, recovered
        cases and deaths for a country from the first recorded case.
    -   `choose_api(type = 2, country_name = "barbados")`  
    -   <https://api.covid19api.com/dayone/country/barbados>
3.  **option2** API, `dayone_status`:
    -   Returns cases by case type for a country from the first recorded
        case. Case types must be one of confirmed, recovered or
        deaths.  
    -   `choose_api(type = "option2", country_name = "barbados", status = "confirmed")`
    -   <https://api.covid19api.com/dayone/country/barbados/status/confirmed>
4.  **option3** API, `dayone_status_live`:
    -   Returns cases by case type for a country from the first recorded
        case with the latest record being the live count. Case types
        must be one of confirmed, recovered or deaths.  
    -   `choose_api(type = "option3", country_name = "barbados", status = "confirmed")`  
    -   <https://api.covid19api.com/dayone/country/barbados/status/confirmed/live>
5.  **option4** API, `datetime_specific_all_status`:
    -   Returns all cases types including confirmed cases, recovered
        cases and deaths for a country. Date and time can vary and be
        chosen by users.  
    -   `choose_api(type = 5, country_name = "barbados", date_from = "2020-05-01", date_to = "2020-05-03", time_from = "00:00:00", time_to = "00:00:00")`
    -   <https://api.covid19api.com/country/barbados?from=2020-05-01T00:00:00Z&to=2020-05-03T00:00:00Z>
6.  **option5** API, `datetime_specific_status`:
    -   Returns cases by case type for a country. Date and time can vary
        and be chosen by users. Case types must be one of confirmed,
        recovered or deaths.  
    -   `choose_api(type = "option5", country_name = "barbados", status = "confirmed", date_from = "2020-05-01", date_to = "2020-05-03", time_from = "00:00:00", time_to = "00:00:00")`
    -   <https://api.covid19api.com/country/barbados/status/confirmed?from=2020-05-01T00:00:00Z&to=2020-05-03T00:00:00Z>
7.  **option6** API, `live_all_status`:
    -   Returns all live cases types including confirmed cases,
        recovered cases and deaths for a country. These records are
        pulled every 10 minutes and are ungrouped.  
    -   `choose_api(type = 7, country_name = "barbados")`  
    -   <https://api.covid19api.com/live/country/barbados>
8.  **option7** API, `live_status`:
    -   Returns live cases by case type for a country. These records are
        pulled every 10 minutes and are ungrouped. Case types must be
        one of confirmed, recovered or deaths.  
    -   `choose_api(type = 8, country_name = "barbados", status = "confirmed")`
    -   <https://api.covid19api.com/live/country/barbados/status/confirmed>
9.  **option8** API, `live_status_after_date`:
    -   Returns live cases by case type for a country after a given
        date/time. These records are pulled every 10 minutes and are
        ungrouped. Case types must be one of confirmed, recovered or
        deaths.  
    -   `choose_api(type = "option8", country_name = "barbados", status = "confirmed", date_from = "2020-05-01", time_from = "00:00:00")`  
    -   <https://api.covid19api.com/live/country/barbados/status/confirmed/date/2020-05-01T00:00:00Z>

Simply use `choose_api(type, ...)` to get the desired API URL. Use
argument, `type = 1` or `type = "general"`, to select the customized API
function call for a summary or a list of available countries or all
data. Use `type = 2` or `type = "option1"` to obtain the API URL for all
cases types for a country since the first recorded case.

If more argument inputs are needed, subsequent order of the arguments
is:

-   “type” -&gt; “country” -&gt; “status” -&gt; “date from” -&gt; “date
    to” -&gt; “time from” -&gt; “time to”

If the desired API URL the users get from the `choose_api` function is
correct, the same exact arguments used in `choose_api` should be used
again in `get_data` function to query the data from Covid-19 API site.
For more information about what the API URL should look like, please
visit the [Covid-19 API
site](https://documenter.getpostman.com/view/10808728/SzS8rjbc).

``` r
choose_api <- function(type, ...) {
  
  switch(type,
         
         general = ref_general(...),
         option1 = dayone_all_status(...),
         option2 = dayone_status(...),
         option3 = dayone_status_live(...),
         option4 = datetime_specific_all_status(...),
         option5 = datetime_specific_status(...),
         option6 = live_all_status(...),
         option7 = live_status(...),
         option8 = live_status_after_date(...)
  )
}

get_data <- function(type, ...){
  
  api_url <- choose_api(type, ...)
  json_data <- GET(api_url)
  extract_data <- rawToChar(json_data$content)
  usable_form <- fromJSON(extract_data)
  usable_form
  
}
```

### 2.2. A List of Input Arguments and their Defaults

The table below show a list of customized API functions with their
corresponding arguments and defaults set.

| Function(s)                    | Argument(s)                                                              | Default(s)                 |
|--------------------------------|--------------------------------------------------------------------------|----------------------------|
| `ref_general`                  | `gen_input`                                                              | None for `gen_input`       |
| `dayone_all_status`            | `country_name`                                                           | `country_name = "BB"`      |
| `dayone_status`                | `country_name`, `status`                                                 | `status = "confirmed"`     |
| `dayone_status_live`           | `country_name`, `status`                                                 | `date_from = "2020-05-01"` |
| `datetime_specific_all_status` | `country_name`, `date_from`, `date_to`, `time_from`, `time_to`           | `date_to = "2020-05-03"`   |
| `datetime_specific_status`     | `country_name`, `status`, `date_from`, `date_to`, `time_from`, `time_to` | `time_from = "00:00:00"`   |
| `live_all_status`              | `country_name`                                                           | `time_to = "00:00:00"`     |
| `live_status`                  | `country_name`, `status`                                                 |                            |
| `live_status_after_date`       | `country_name`, `status`, `date_from`, `time_from`                       |                            |

Let’s look at the options each argument can take:

1.  `gen_input`:
    -   Can take or partially match these options, `"summary"`,
        `"countries"`, `"all"` (or “Su”, “SuM”, “sUmMa”). Non-case
        sensitive.  
    -   I did not set a default value for `gen_input` function because
        using the API URL generated by this function will query a large
        data set and will be time-consuming. Therefore, unless the users
        really want to query from these options, I do not want them to
        accidentally set it off. In a word, if the users really want to
        query from these options, they can input those arguments
        themselves. Otherwise, I would recommend to go to the [Covid-19
        API
        site](https://documenter.getpostman.com/view/10808728/SzS8rjbc#00030720-fae3-4c72-8aea-ad01ba17adf8)
        to see the example lists from these options.
2.  `country_name`:
    -   Must be the slug country name from the available list or
        2-letter (ISO2) or 3-letter (ISO3) abbreviations.  
    -   For example, “uNitED-sTATes”, “UnItEd StAtES”, “uSa” and “uS”
        are all valid inputs. Non-case sensitive.  
    -   Default country is Barbados.
3.  `status`:
    -   Can take or partially match these options, `"confirmed"`,
        `"recovered"`, `"deaths"` (or “Co”, “cOn”, “cOnf”). Non-case
        sensitive.  
    -   Default status is “confirmed”.
4.  `date_from` and `date_to`:
    -   Can take “2020 12 31”, “12/31/2021”, “2020-12-31”, “3 20 2021”,
        “2020 10 6”.  
    -   Note: If users request data from dates that are before Covid-19
        time for a country, i.e. 2018, this may result in an error or
        the data contains all zero lists.  
    -   Default for `date_from` is “2020-05-01” and for `date_to` is
        “2020-05-03”.
5.  `time_from` and `time_to`:
    -   *Hour* can take any numeric values from 0 to 23 (must be in
        military time).  
    -   *Minute* can take any numeric values from 0 to 59.  
    -   *second* can take any numeric values from 0 to 59.  
    -   Can take “1:4:5”, “1/4/5”, “1-4-5” (all equivalent to
        “01:04:05”).  
    -   Default for both `time_from` and `time_to` is "00:00:00:

**Note: If users do not supply any argument inputs but simply choose an
option function to guery the data, it will return cases results for
Barbados from May 1st, 2020, at time 00:00:00 to May 3rd, 2020, at time
00:00:00.**

**Note 2: Free API query does not require a key for the Covid-19 API
site. However, free queries have limited access and functionality. For
example, free queries can only query up to a week of data at once. So,
if you need to query a month of data. you will have to run the query
four times with different date inputs.**

### 2.3 Using an API

Below is the API URL used with `get_data` function I created to query
the data for all cases type for the United States from January 3rd, 2021
to January 5th, 2021 (3 days). We can see it returns for 10,000 rows and
13 columns including every city from every state in the United States
for 3 days worth of all cases type of data. I realized by looking at
this example data that all cases types are accumulated up untill the
date/time queried. If I want to calculate all cases types for each month
for each state in the United States, I can query one day of data at the
end of each month, group by each state and sum up all different cases
type, subtract them from the previous end of month one day data. Then,
we will have the number of all cases types per state per month, not
cumulatively. Let’s see if this works!

``` r
choose_api(5, "usa", date_from="2021 1 3", date_to="2021 1 5", time_to="23:59:59")
```

    ## [1] "https://api.covid19api.com/country/united-states?from=2021-01-03T00:00:00Z&to=2021-01-05T23:59:59Z"

``` r
test_data <- get_data(5, "usa", date_from="2021 1 3", date_to="2021 1 5", time_to="23:59:59")
test_data <- test_data %>% group_by(City)
test_data
```

    ## # A tibble: 10,011 x 13
    ##    ID                                   Country  CountryCode Province  City  CityCode Lat   Lon   Confirmed Deaths Recovered Active Date 
    ##    <chr>                                <chr>    <chr>       <chr>     <chr> <chr>    <chr> <chr>     <int>  <int>     <int>  <int> <chr>
    ##  1 00055249-20ac-45e9-a173-7b306e8acaf0 United ~ US          Texas     Hend~ 48213    32.21 -95.~      3675     78         0   3597 2021~
    ##  2 000f161d-225d-43cc-baf4-7c0ba49423b1 United ~ US          New Mexi~ Torr~ 35057    34.64 -105~       498      4         0    494 2021~
    ##  3 00591a34-171d-43b5-9389-21d7ddec8ab7 United ~ US          Oklahoma  Pitt~ 40121    34.92 -95.~      3184     25         0   3159 2021~
    ##  4 009f4dd8-e8fe-4cbe-b667-ada7cb68d23e United ~ US          Arkansas  Mill~ 5091     33.31 -93.~      2818     25         0   2793 2021~
    ##  5 00b299cb-febb-4eee-875c-2e814861b2cb United ~ US          Mississi~ Tate  28137    34.65 -89.~      2441     56         0   2385 2021~
    ##  6 00d8ef35-c084-4e4a-b454-528066123d41 United ~ US          Illinois  Wayne 17191    38.43 -88.~      1358     40         0   1318 2021~
    ##  7 00eb8717-82f7-4cbc-927e-ad1dc5f27c0d United ~ US          Virginia  Carr~ 51035    36.73 -80.~      1534     36         0   1498 2021~
    ##  8 0105b524-ad7b-4abd-bef5-7fa28d94a6c9 United ~ US          South Da~ Auro~ 46003    43.72 -98.~       414      8         0    406 2021~
    ##  9 0105d892-7d39-4015-b9b7-18fbabc47ae6 United ~ US          Tennessee Laud~ 47097    35.76 -89.~      2702     29         0   2673 2021~
    ## 10 012b834c-2e62-44b4-9788-838a07bdf369 United ~ US          New Jers~ Camd~ 34007    39.8  -74.~     32243    833         0  31410 2021~
    ## # ... with 10,001 more rows

## 3. Getting the Data

### 3.1. Combining Date Sets

Since the free Covid-19 data query does not require a key and has
limited access/queries. For instance, each time we make a request to
query, we can only query data for a range up to a week. Thus, I made a
single-day query for the last day of each month from December 2020 to
September 2021 since the data is accumulated. I only selected five
columns (*Province*, *Confirmed*, *Deaths*, *Active*, *Date*) from each
query and saved them to an object named by a month of that query. I
row-combined those objects/queries into two different data sets since
they all have the same column names. This way I did so that I can
column-combine them later and subtract the same case type for each state
for each month.

The data set I had in mind eventually should look like the example table
shown below so that I can compute the following tasks.

-   Total confirmed cases/month, state = Confirmed\_cases -
    Confirmed\_cases2  
-   Total number of deaths/month, state = Deaths\_1 - Deaths\_2 …

| Province       | Confirmed\_cases | Confirmed\_cases2 | Deaths\_1      | Deaths\_2      | Month    |
|----------------|------------------|-------------------|----------------|----------------|----------|
| North Carolina | 4569 (Jan 2021)  | 4123 (Dec 2020)   | 568 (Jan 2021) | 538 (Dec 2020) | January  |
| North Carolina | 5638 (Feb 2021)  | 4569 (Jan 2021)   | 588 (Feb 2021) | 568 (Jan 2021) | February |
| North Carolina | 5883 (Mar 2021)  | 5638 (Feb 2021)   | 638 (Mar 2021) | 588 (Feb 2021) | March    |

…

``` r
dec <- get_data(5, "usa", date_from="2020 12 31", date_to="2020 12 31", time_to="23:59:59")
jan <- get_data(5, "usa", date_from="2021 1 31", date_to="2021 1 31", time_to="23:59:59")
feb <- get_data(5, "usa", date_from="2021 2 28", date_to="2021 2 28", time_to="23:59:59")
mar <- get_data(5, "usa", date_from="2021 3 31", date_to="2021 3 31", time_to="23:59:59")
apr <- get_data(5, "usa", date_from="2021 4 30", date_to="2021 4 30", time_to="23:59:59")
may <- get_data(5, "usa", date_from="2021 5 31", date_to="2021 5 31", time_to="23:59:59")
jun <- get_data(5, "usa", date_from="2021 6 30", date_to="2021 6 30", time_to="23:59:59")
jul <- get_data(5, "usa", date_from="2021 7 31", date_to="2021 7 31", time_to="23:59:59")
aug <- get_data(5, "usa", date_from="2021 8 31", date_to="2021 8 31", time_to="23:59:59")
sep <- get_data(5, "usa", date_from="2021 9 30", date_to="2021 9 30", time_to="23:59:59")

dec_2 <- dec %>% select(Province, Confirmed, Deaths, Active, Date) 
jan_2 <- jan %>% select(Province, Confirmed, Deaths, Active, Date)
feb_2 <- feb %>% select(Province, Confirmed, Deaths, Active, Date)
mar_2 <- mar %>% select(Province, Confirmed, Deaths, Active, Date)
apr_2 <- apr %>% select(Province, Confirmed, Deaths, Active, Date)
may_2 <- may %>% select(Province, Confirmed, Deaths, Active, Date)
jun_2 <- jun %>% select(Province, Confirmed, Deaths, Active, Date)
jul_2 <- jul %>% select(Province, Confirmed, Deaths, Active, Date)
aug_2 <- aug %>% select(Province, Confirmed, Deaths, Active, Date)
sep_2 <- sep %>% select(Province, Confirmed, Deaths, Active, Date)

late_date <- rbind(jan_2, feb_2, mar_2, apr_2, may_2, jun_2, jul_2, aug_2, sep_2)
early_date <- rbind(dec_2, jan_2, feb_2, mar_2, apr_2, may_2, jun_2, jul_2, aug_2)
```

## 4. Data Manipulation

At this point, I have data from every city in each state. So, I grouped
by *Province* and *Date* and sum up all different case types for each
state for the same date. I applied this to both data sets. Then, I
changed the names of the columns in one of the datasets in order to
perform a column-combination of the two tables afterwards and saved it
as an object, *mydata1*. Now, *mydata1* is in a format I had in mind
earlier as shown in the table above.

``` r
after_date <- late_date %>% group_by(Province, Date) %>% summarise(across(c(Confirmed, Deaths, Active), sum))
before_date <- early_date %>% group_by(Province, Date) %>% summarise(across(c(Confirmed, Deaths, Active), sum))

names(before_date) <- c("State", "Date2", "Confirmed2", "Deaths2", "Active2")
mydata1 <- cbind(after_date, before_date)
alldata <- mydata1[1:9, ]
mydata1 <- mydata1[-1:-9, ]
mydata1
```

    ## # A tibble: 522 x 10
    ##    Province Date                 Confirmed Deaths Active State   Date2                Confirmed2 Deaths2 Active2
    ##    <chr>    <chr>                    <int>  <int>  <int> <chr>   <chr>                     <int>   <int>   <int>
    ##  1 Alabama  2021-01-31T00:00:00Z    459639   7688 451951 Alabama 2020-12-31T00:00:00Z     361226    4827  356399
    ##  2 Alabama  2021-02-28T00:00:00Z    493252   9929 483323 Alabama 2021-01-31T00:00:00Z     459639    7688  451951
    ##  3 Alabama  2021-03-31T00:00:00Z    515388  10554 504834 Alabama 2021-02-28T00:00:00Z     493252    9929  483323
    ##  4 Alabama  2021-04-30T00:00:00Z    527922  10896 517026 Alabama 2021-03-31T00:00:00Z     515388   10554  504834
    ##  5 Alabama  2021-05-31T00:00:00Z    543405  11146 532259 Alabama 2021-04-30T00:00:00Z     527922   10896  517026
    ##  6 Alabama  2021-06-30T00:00:00Z    550983  11352 539631 Alabama 2021-05-31T00:00:00Z     543405   11146  532259
    ##  7 Alabama  2021-07-31T00:00:00Z    585607  11536 574071 Alabama 2021-06-30T00:00:00Z     550983   11352  539631
    ##  8 Alabama  2021-08-31T00:00:00Z    699729  12283 687446 Alabama 2021-07-31T00:00:00Z     585607   11536  574071
    ##  9 Alabama  2021-09-30T00:00:00Z    796475  14299 782176 Alabama 2021-08-31T00:00:00Z     699729   12283  687446
    ## 10 Alaska   2021-01-31T00:00:00Z     54350    262  54088 Alaska  2020-12-31T00:00:00Z      47014     206   46808
    ## # ... with 512 more rows

### 4.1. Creating Numeric Variables

Now we are ready to subtract each case type from itself from the
previous month and obtain the total number of confirmed cases, active
cases and deaths for each month for each state. Then, we can compare
those numbers between different states.

Four variables are created here, and we do not keep the old variables we
used to compute the new ones. These variables are now representative
numbers for the corresponding month and state. They are no longer the
cumulative figures.

-   *Total*  
-   *Month*  
-   *Deaths*  
-   *Active*

You can also save this data to your local drive in case of an unexpected
event occurred such as website maintenance.

``` r
mydata2 <- mydata1 %>% mutate(Total = Confirmed - Confirmed2, Death = Deaths - Deaths2, Active_cases = Active - Active2, 
                              Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) %>% 
  select(State, Month, Total, Death, Active_cases)
```

    ## Adding missing grouping variables: `Province`

``` r
names(mydata2) <- c("Province","State", "Month", "Total", "Deaths", "Active")
mydata2
```

    ## # A tibble: 522 x 6
    ##    Province State   Month  Total Deaths Active
    ##    <chr>    <chr>   <chr>  <int>  <int>  <int>
    ##  1 Alabama  Alabama Jan    98413   2861  95552
    ##  2 Alabama  Alabama Feb    33613   2241  31372
    ##  3 Alabama  Alabama Mar    22136    625  21511
    ##  4 Alabama  Alabama Apr    12534    342  12192
    ##  5 Alabama  Alabama May    15483    250  15233
    ##  6 Alabama  Alabama Jun     7578    206   7372
    ##  7 Alabama  Alabama Jul    34624    184  34440
    ##  8 Alabama  Alabama Aug   114122    747 113375
    ##  9 Alabama  Alabama Sep    96746   2016  94730
    ## 10 Alaska   Alaska  Jan     7336     56   7280
    ## # ... with 512 more rows

``` r
# write_csv(x = mydata2, path = "../_Data/covid_data.csv")
```

### 4.2. Creating Categorical Variables

Upon inspecting the data, I discovered eight states I wanted to be
excluded from the data analysis. So, I created an infix function,
`%!in%`, that is negation of the infix function, `%in%`. I used it to
filter the rows whenever *Province* contains those states. These states
I excluded from the data are: “American Samoa”, “Diamond Princess”,
“Grand Princess”, “Guam”, “Northern Mariana Islands”, “Puerto Rico” and
“Virgin Islands”. They may be potential outliers since their number of
cases may be really low for each case type.

I used `mutate` function to create three categorical variables using the
numerical variables we just obtained.

-   *f500\_deaths* (2 levels):
    -   Less than 500 deaths  
    -   More than 500 deaths
-   *Total\_cases* (3 levels):
    -   Less than 9,000 cases  
    -   Between 9,000 and 30,000 cases  
    -   More than 30,000 cases
-   *vaccine* (3 levels):
    -   Vaccinat-ING (Jan, Feb, Mar, Apr)  
    -   Some vaccinated (May, Jun, Jul)  
    -   Back to school (Aug, Sep)

``` r
`%!in%` <- Negate(`%in%`)

mydata3 <- mydata2 %>% filter(Province %!in% c("American Samoa", "Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands", " ")) %>% 
  mutate(f500_deaths = if_else(Deaths < 500, 1, 2), 
         Total_cases = if_else(Total < 9000, 1, 
                             if_else(Total < 30000, 2, 3)), 
         vaccine = if_else(Month %in% c("Jan", "Feb", "Mar", "Apr"), 1, 
                           if_else(Month %in% c("May", "Jun", "Jul"), 2, 3)))

mydata3$f500_deaths <- cut(mydata3$f500_deaths, 2, c("Less than 500 deaths", "More than 500 deaths"))
mydata3$Total_cases <- cut(mydata3$Total_cases, 3, c("Less than 9,000 cases", "Between 9,000 and 30,000 cases", "More than 30,000 cases"))
mydata3$vaccine <- cut(mydata3$vaccine, 3, c("Vaccinat-ING", "Some vaccinated", "Back to school"))
```

## 5. Exploratory Data Analysis

### 5.1. Contingency Tables

Now we have 51 states, and each state has nine months worth of data.
Therefore, 51 x 9 = 459 rows in the data. Each event in Table 1
represents one state and one month. However, since “vaccinat-ing” period
has 4 months, “Some vaccinated” period contains 3 months and “Back to
school” period only has 2 months, the data is not on a even scale for
the three categories. Hence, we cannot compare them across columns. We
can only compare them down the rows within each column.

During the “vaccinat-ing” period (Jan-Apr), most of the states appeared
with more than 30,000 cases per month. However, that number dropped
during “Some vaccinated” period (May-Jul). A lot of the events
(month&state) were having less than 9,000 cases. This incident should
tell us that the Covid-19 vaccines were working its magic during this
time. Unfortunately, the number of events went back up to having more
than 30,000 cases during “Back to school” period when students were
going back to classrooms, in-person delivery teaching method.

Table 2 and Table 3 show the total confirmed cases and total number of
deaths between the states known to have the most cases from January to
September 2021 compared to North Carolina. One interesting finding is
that the number of total confirmed cases were slowly dropping for each
state during “vaccinat-ing” period and then reached the minimum number
of cases of all time during “some vaccinated” period. However, that
number went back up when students went back to schools. The number total
confirmed cases were almost equivalent to those numbers back in January
for most states. This is why state of North Carolina reinforced the mask
mandate again in August 2021.

Since there are more cases in each state, there will be more deaths. If
the number of cases are less, the number of deaths will be less in each
state as well. It is difficult to see this in a table. Let’s visualize
these data with different plots in the plots section.

``` r
# 1st contingency table
table(mydata3$Total_cases, mydata3$vaccine) %>% kable(caption = "Table 1. Total Confirmed Cases vs. Vaccine Timeline")
```

|                                | Vaccinat-ING | Some vaccinated | Back to school |
|:-------------------------------|-------------:|----------------:|---------------:|
| Less than 9,000 cases          |           46 |              79 |             10 |
| Between 9,000 and 30,000 cases |           63 |              51 |             24 |
| More than 30,000 cases         |           95 |              23 |             68 |

Table 1. Total Confirmed Cases vs. Vaccine Timeline

``` r
# 2nd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Deaths) %>%
  pivot_wider(names_from = "Month", values_from = "Deaths") %>% arrange(desc(Jan)) %>% 
  kable(caption = "Table 2. Number of Deaths for Each Month across Different States")
```

| Province       |   Jan |   Feb |  Mar |  Apr |  May | Jun |  Jul |  Aug |   Sep |
|:---------------|------:|------:|-----:|-----:|-----:|----:|-----:|-----:|------:|
| California     | 14994 | 11621 | 6373 | 2463 | 1263 | 813 |  888 | 1448 |  3257 |
| Texas          |  9008 |  6623 | 4555 | 1967 | 1289 | 829 |  940 | 3767 |  8159 |
| New York       |  5651 |  3967 | 2680 | 1958 | 1043 | 372 |  171 |  613 |  1104 |
| Florida        |  4806 |  4373 | 2573 | 1736 | 1613 | 998 | 1307 | 5482 | 10448 |
| North Carolina |  2587 |  1877 |  900 |  539 |  427 | 359 |  198 |  833 |  2056 |
| Michigan       |  2489 |   977 |  610 | 1629 | 1627 | 620 |  187 |  359 |   808 |

Table 2. Number of Deaths for Each Month across Different States

``` r
# 3rd contingency table
mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  select(Province, Month, Total) %>%
  pivot_wider(names_from = "Month", values_from = "Total") %>% arrange(desc(Jan)) %>% 
  kable(caption = "Table 3. Total Confirmed Cases for Each Month across Different States")
```

| Province       |    Jan |    Feb |    Mar |    Apr |   May |   Jun |    Jul |    Aug |    Sep |
|:---------------|-------:|-------:|-------:|-------:|------:|------:|-------:|-------:|-------:|
| California     | 997110 | 241898 | 100463 |  76039 | 49031 | 29059 | 148305 | 432125 | 268073 |
| Texas          | 603560 | 276669 | 138897 | 102018 | 59481 | 46660 | 138768 | 473409 | 456018 |
| New York       | 441124 | 223960 | 229271 | 175012 | 54254 | 12582 |  35404 | 128200 | 148093 |
| Florida        | 398062 | 187844 | 148514 | 175783 | 87300 | 44646 | 268770 | 616220 | 332353 |
| North Carolina | 217981 | 101022 |  55584 |  55620 | 31402 | 12535 |  34387 | 165578 | 181600 |
| Michigan       |  78775 |  34297 |  99685 | 189525 | 55110 |  8209 |  11064 |  49187 |  89646 |

Table 3. Total Confirmed Cases for Each Month across Different States

### 5.2. Numerical Summaries

Table 4 and Table 5 shows the numerical summaries of active cases and
deaths during each vaccine timeline for North Carolina and its
neighbors. North Carolina was placed pretty high regarding to the active
cases for each timeline right below Georgia, followed by Tennessee.
Shockingly, the average active cases for “back to school” period all
exceeded the average active cases for “vaccinat-ing” period for every
state with Virginia being a close-call. The number was almost doubled
for some states or more than doubled for Tennessee. This indicates that
“back to school” event drove the number of cases up in each state and
counterattacked the great effect brought by wide-spread vaccine
administration. The effect of vaccine is shown in the “some vaccinated”
period that drove the average active cases down.

However, the effect of “going back to school” event is not as dramatic
as shown in Table 4 as in Table 5. The number of deaths did go up during
“back to school” period due to elevated number of active cases in the
same period, however, that number was not almost doubled or doubled.
They were only matching or even. This could be the vaccines were
developing antibodies and immunity in our body, and hence, the infected
individuals had a better chance to fight the virus and survived during
“back to school” period. People should really get their vaccine shots if
they have not already.

``` r
mydata3 %>% filter(Province %in% c("North Carolina", "South Carolina", "Tennessee","Georgia", "Virginia")) %>% 
  group_by(Province, vaccine) %>% 
  summarise(Avg_active = mean(Active), SD_active = sd(Active), Median_active = median(Active), IQR_active = IQR(Active)) %>% 
  kable(digit = 2, caption = "Table 4. Active cases per vaccine timeline for North Carolina and its neighbors.")
```

| Province       | vaccine         | Avg\_active | SD\_active | Median\_active | IQR\_active |
|:---------------|:----------------|------------:|-----------:|---------------:|------------:|
| Georgia        | Vaccinat-ING    |   106119.75 |   92096.53 |        72623.0 |    82090.25 |
| Georgia        | Some vaccinated |    25925.33 |   17422.78 |        23007.0 |    17238.50 |
| Georgia        | Back to school  |   197429.50 |   36259.73 |       197429.5 |    25639.50 |
| North Carolina | Vaccinat-ING    |   106076.00 |   75806.97 |        77113.0 |    73225.50 |
| North Carolina | Some vaccinated |    25780.00 |   11890.50 |        30975.0 |    11006.50 |
| North Carolina | Back to school  |   172144.50 |   10464.47 |       172144.5 |     7399.50 |
| South Carolina | Vaccinat-ING    |    66737.75 |   49099.03 |        53070.0 |    55158.25 |
| South Carolina | Some vaccinated |    12694.00 |    8488.12 |        14364.0 |     8364.00 |
| South Carolina | Back to school  |   118835.00 |    1965.76 |       118835.0 |     1390.00 |
| Tennessee      | Vaccinat-ING    |    63834.50 |   49860.35 |        40863.5 |    32533.00 |
| Tennessee      | Some vaccinated |    15199.00 |   11000.60 |        14727.0 |    10993.00 |
| Tennessee      | Back to school  |   166274.00 |   12406.90 |       166274.0 |     8773.00 |
| Virginia       | Vaccinat-ING    |    76067.00 |   53515.94 |        55204.5 |    49384.50 |
| Virginia       | Some vaccinated |    11344.00 |    5918.52 |        13927.0 |     5479.50 |
| Virginia       | Back to school  |    85587.00 |   19581.20 |        85587.0 |    13846.00 |

Table 4. Active cases per vaccine timeline for North Carolina and its
neighbors.

``` r
mydata3 %>% filter(Province %in% c("North Carolina", "South Carolina", "Tennessee","Georgia", "Virginia")) %>% 
  group_by(Province, vaccine) %>% 
  summarise(Average = mean(Deaths), SD = sd(Deaths), Median = median(Deaths), IQR = IQR(Deaths)) %>% 
  kable(digit = 2, caption = "Table 5. Number of deaths per vaccine timeline for North Carolina and its neighbors.")
```

| Province       | vaccine         | Average |      SD | Median |     IQR |
|:---------------|:----------------|--------:|--------:|-------:|--------:|
| Georgia        | Vaccinat-ING    | 2314.00 | 1034.82 | 2428.5 | 1535.00 |
| Georgia        | Some vaccinated |  495.33 |  212.07 |  586.0 |  197.00 |
| Georgia        | Back to school  | 2204.00 | 1612.20 | 2204.0 | 1140.00 |
| North Carolina | Vaccinat-ING    | 1475.75 |  931.82 | 1388.5 | 1244.75 |
| North Carolina | Some vaccinated |  328.00 |  117.61 |  359.0 |  114.50 |
| North Carolina | Back to school  | 1444.50 |  864.79 | 1444.5 |  611.50 |
| South Carolina | Vaccinat-ING    | 1050.25 |  678.63 | 1052.0 | 1026.75 |
| South Carolina | Some vaccinated |  135.67 |   89.49 |   85.0 |   78.00 |
| South Carolina | Back to school  | 1284.00 |  834.39 | 1284.0 |  590.00 |
| Tennessee      | Vaccinat-ING    | 1322.50 | 1148.63 | 1127.0 | 1563.50 |
| Tennessee      | Some vaccinated |  175.67 |   62.63 |  162.0 |   61.50 |
| Tennessee      | Back to school  | 1209.00 |  643.47 | 1209.0 |  455.00 |
| Virginia       | Vaccinat-ING    | 1434.50 |  667.72 | 1566.0 |  593.50 |
| Virginia       | Some vaccinated |  254.00 |  151.06 |  229.0 |  149.50 |
| Virginia       | Back to school  |  609.00 |  422.85 |  609.0 |  299.00 |

Table 5. Number of deaths per vaccine timeline for North Carolina and
its neighbors.

### 5.3. Visualization

#### 5.3.1. Bar-Plots

We combined with the findings we learnt from Table 1, Table 2 and Table
3 and showed them in Figure 1 and Figure 2 below.

Figure 1 and Figure 2 show the average number of active cases and deaths
at each vaccine timeline for the states known to have the most cases.
Comparing both figures, relatively speaking, even when “back to school”
event took its effect on the increased number of active cases during the
same period, the average number of deaths dropped at the same period for
most states but not for Florida. This could be due to its residents
being mostly retired individuals in Florida, and thus, the average age
of the residents is higher than most states in Florida. Older
individuals normally have lower immunity despite vaccine shots, and they
face a lower than average odds again the virus. However, with the
average number of active cases increased in the “back to school” period
and exceeded the number in the first period for California, Florida,
North Carolina and Texas, the average number of deaths dropped in the
third period relative to the average number of deaths in the first
period for California, North Carolina and Texas.

Again, the wide-spread vaccine administration across US is working, and
those individuals who received shots are developing antibodies against
the virus. Thus, the average number of deaths was not elevated for most
states even when students were going back to school.

``` r
barplot1 <- mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  group_by(Province, vaccine) %>% 
  summarise(Avg_active = mean(Active), SD_active = sd(Active), Median_active = median(Active), IQR_active = IQR(Active))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
barplot2 <- mydata3 %>% filter(Province %in% c("California", "New York", "Florida", "Texas", "Michigan", "North Carolina")) %>% 
  group_by(Province, vaccine) %>% 
  summarise(Average = mean(Deaths), SD = sd(Deaths), Median = median(Deaths), IQR = IQR(Deaths))
```

    ## `summarise()` has grouped output by 'Province'. You can override using the `.groups` argument.

``` r
g <- ggplot(data = barplot1, aes(x = Province, y = Avg_active, fill = vaccine))
g + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Average Active Cases", title = "Figure 1. Average active cases at each timeline for each state") + 
  scale_fill_discrete(name = "Vaccine timeline") + 
  theme(axis.text.x = element_text(angle = 45, size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-11-1.png)<!-- -->

``` r
d <- ggplot(data = barplot2, aes(x = Province, y = Average, fill = vaccine))
d + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Average Deaths", title = "Figure 2. Average deaths at each timeline for each state") + 
  scale_fill_discrete(name = "Vaccine timeline") + 
  theme(axis.text.x = element_text(angle = 45, size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 13), 
        axis.title.y = element_text(size = 13), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-11-2.png)<!-- -->

#### 5.3.2. Box-Plots

Figure 3 and Figure 4 show the entire US data of active cases and deaths
at each timeline. Note that each state has four dots in “vaccinat-ing”
period, three dots in “some vaccinated” period and two dots in “back to
school” period. So, there will be more data points in the first period
than the second period and the third period. In addition, I apologize
that due to the numerous outliers in both boxplots, I had to adjust the
y-axis scales in order to see the median for each timeline.

Overall, Figure 3 shows that the median active cases is higher in the
“back to school” period than in the “vaccinat-ing” period or “some
vaccinated” period in US. The IQR block seems to be longer in the third
period than it is in the first period. Again, we do not have enough
information for the “back to school” period to confirm this. However,
the median active cases is definitely the lowest and the IQR block is
the smallest in the “some vaccinated” period in US compared to the other
two timelines.

Figure 4 shows the median deaths is about the same in both
“vaccinat-ing” period and “back to school” period with “back to school”
IQR block being a little smaller than the “vaccinat-ing” IQR block. We
need more information in the “back to school” period to be definite
about this.

``` r
boxplot1 <- ggplot(data = mydata3, aes(x = vaccine, y = Active))
boxplot1 + geom_boxplot(fill = "white", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0, 200000)) + 
  geom_jitter(aes(color = vaccine), size = 3) + 
  labs(x = "Vaccine Timeline", y = "Active cases", title = "Figure 3. Active cases at each timeline in US") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 14))
```

![](/images/unnamed-chunk-12-1.png)<!-- -->

``` r
boxplot2 <- ggplot(data = mydata3, aes(x = vaccine, y = Deaths))
boxplot2 + geom_boxplot(fill = "white", outlier.shape = NA) + 
  coord_cartesian(ylim=c(0, 4000)) + 
  geom_jitter(aes(color = vaccine), size = 3) + 
  labs(x = "Vaccine Timeline", y = "Number of Deaths", title = "Figure 4. Total number of deaths at each timeline in US") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 14))
```

![](/images/unnamed-chunk-12-2.png)<!-- -->

#### 5.3.3. Scatterplot

Figure 5 scatterplot shows the relationship between number of active
cases and number of deaths for different timelines in US. We can already
guess that the relationship between active cases and deaths is a strong
positive correlation. However, will that correlation stay the same
between them for all vaccine time groups? Apparently not! Figure 5
indicates that in the “vaccinat-ing” period, the correlation between
active cases and deaths is the strongest compared to the other two time
groups. However, when the vaccine shots were taken place across US,
approximately 40% of the population were vaccinated and together with
the State-mandated restrictions during “some vaccinated” period, despite
the increasing active cases, deaths were not imminent in this period.
The active cases and number of deaths have the weakest correlation in
the “some vaccinated” period.

Furthermore, despite also increased number of active cases in the “back
to school” period, the correlation between number of deaths and active
cases is still not nearly as strong as they are in the “vaccinat-ing”
period. Table 6, the correlation table, coincides with our findings
shown in Figure 5.

Once again, the plot is showing vaccines are helping the population to
fight the virus. Despite students going back to schools and the more
contagious and virulent Delta variant, the infected but fully vaccinated
individuals may have better surviving odds against the virus. I
apologize again that due to some large number outliters, I have to
zoom-in on the action part to see the relationships better.

``` r
sub1 <- mydata3 %>% filter(vaccine %in% c("Vaccinat-ING"))
sub2 <- mydata3 %>% filter(vaccine %in% c("Some vaccinated"))
sub3 <- mydata3 %>% filter(vaccine %in% c("Back to school"))

corr1 <- cor(sub1$Active, sub1$Deaths)
corr2 <- cor(sub2$Active, sub2$Deaths)
corr3 <- cor(sub3$Active, sub3$Deaths)
corr_all <- cbind(corr1, corr2, corr3)
kable(corr_all, digits = 4, col.names = c("Vaccinat-ING", "Some vaccinated", "Back to school"), 
      caption = "Table 6, Correlations between active cases and deaths for each group")
```

| Vaccinat-ING | Some vaccinated | Back to school |
|-------------:|----------------:|---------------:|
|       0.8814 |          0.6761 |         0.7897 |

Table 6, Correlations between active cases and deaths for each group

``` r
scatter <- ggplot(data = mydata3, aes(x = Active, y = Deaths, color = vaccine))
scatter + geom_point(aes(shape = f500_deaths),size = 3) + 
  scale_shape_discrete(name = "500 deaths") + 
  coord_cartesian(xlim=c(0, 300000), ylim=c(0, 7500)) +
  geom_smooth(method = lm, lwd = 2) + 
  labs(x = "Active Cases", y = "Number of Deaths", title = "Figure 5. Active cases vs Deaths for each Vaccine timeline in US") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](/images/unnamed-chunk-13-1.png)<!-- -->

``` r
#scale_fill_discrete(name = NULL)
```

#### 5.3.4.Histogram

Figure 6 shows histograms of number of deaths in US at different
timelines. This may not be as helpful to us as the other plots. Remember
there is one issue we addressed in the boxplots section. That is, there
are twice as much data points in the “vaccinat-ing” period than in the
“back to school” period since the “vaccinat-ing” period has four months
of data for each state while the “back to school” period only has two
months of data. Regardless, we see a significantly low deaths count in
the “some vaccinated” period than the other two timelines. This says
with approximately 40% of US population being vaccinated at least one
shot and with the most state-mandated regulations on Covid-19
restrictions in the “some-vaccinated” period, we had a low deaths count
in the same period.

``` r
his <- ggplot(data = mydata3, aes(x = Deaths, color = vaccine, fill = vaccine))
his + geom_histogram() + 
  coord_cartesian(xlim=c(0, 5000)) + 
  labs(title = "Figure 6. Number of deaths in US for each vaccine timeline") + 
  theme(axis.text.x = element_text(angle= 45, size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13)) + 
  facet_wrap(~vaccine) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](/images/unnamed-chunk-14-1.png)<!-- -->

#### 5.3.5. Line Plots

Figure 7 shows the total number of active cases each month in North
Carolina and its neighbors since January 2021. The plot indicates the
numbers were decreasing each month since January for every state and
finally reached the minimum number of active cases so far for every
state in June 2021. Then, the numbers were slowly going back up but then
elevated when students went back to classroom and most states relaxed on
their Covid-19 restrictions such as mask mandate and social distancing.
This is reflected in the numerical summary shown in Table 4.

Figure 8 shows the total number of deaths each month in North Carolina
and its neighbors since January 2021. This plot displays similar pattern
as shown in Figure 7. Relatively speaking, North Carolina and some of
its neighbors have less deaths in the later months than in the earlier
months of 2021. Antibodies from the vaccine are helping the infected but
vaccinated individuals to fight the virus. This is also reflected in the
numerical summary shown in Table 5.

These plots tell us that despite the high-volume vaccine administration
during the earlier months in 2021, State-mandated regulations on
Covid-19 restrictions also helped greatly drive the number of cases and
deaths down. Thus, we should continue to do so to help stop the
community spread of the virus and get vaccine shots if people have not
already.

``` r
mydata3$Month <- factor(mydata3$Month, levels = unique(mydata3$Month))
neighbor <- mydata3 %>% filter(State %in% c("North Carolina", "South Carolina", "Tennessee","Georgia", "Kentucky", "Virginia"))

lineplot1 <- ggplot(data = neighbor, aes(x = Month, y = Active, color = State))
lineplot1 + geom_line(aes(group = State), lwd = 2) + geom_point() + 
  labs(y = "Active Cases", title = "Figure 7. Active cases each month in NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-15-1.png)<!-- -->

``` r
lineplot2 <- ggplot(data = neighbor, aes(x = Month, y = Deaths, color = State))
lineplot2 + geom_line(aes(group = State), lwd = 2) + geom_point() + 
  labs(y = "Total Deaths", title = "Figure 8. Number of deaths each month in NC and its neighbors") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 13), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-15-2.png)<!-- -->

## 6. Conclusion

``` r
alldata
```

    ## # A tibble: 9 x 10
    ##   Province Date                 Confirmed Deaths   Active State Date2                Confirmed2 Deaths2  Active2
    ##   <chr>    <chr>                    <int>  <int>    <int> <chr> <chr>                     <int>   <int>    <int>
    ## 1 ""       2021-01-31T00:00:00Z  26300968 449198 25799922 ""    2020-12-31T00:00:00Z   20153407  352001 19748034
    ## 2 ""       2021-02-28T00:00:00Z  28701621 514823 28136320 ""    2021-01-31T00:00:00Z   26300968  449198 25799922
    ## 3 ""       2021-03-31T00:00:00Z  30514599 552328 29912496 ""    2021-02-28T00:00:00Z   28701621  514823 28136320
    ## 4 ""       2021-04-30T00:00:00Z  32403009 576119 31774444 ""    2021-03-31T00:00:00Z   30514599  552328 29912496
    ## 5 ""       2021-05-31T00:00:00Z  33321318 594293 32673092 ""    2021-04-30T00:00:00Z   32403009  576119 31774444
    ## 6 ""       2021-06-30T00:00:00Z  33720689 604620 33060180 ""    2021-05-31T00:00:00Z   33321318  594293 32673092
    ## 7 ""       2021-07-31T00:00:00Z  35037755 613361 34365119 ""    2021-06-30T00:00:00Z   33720689  604620 33060180
    ## 8 ""       2021-08-31T00:00:00Z  39321999 641224 38557964 ""    2021-07-31T00:00:00Z   35037755  613361 34365119
    ## 9 ""       2021-09-30T00:00:00Z  43460343 698395 42761967 ""    2021-08-31T00:00:00Z   39321999  641224 38557964

``` r
overall <- alldata %>% mutate(Active_cases = Active - Active2, Death = Deaths - Deaths2, Total = Confirmed - Confirmed2, 
                   Month = rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))) 
overall$Month <- factor(overall$Month, levels = unique(overall$Month))

lineplot3 <- ggplot(data = overall, aes(x = Month, y = Active_cases, group = 1))
lineplot3 + geom_line(lwd = 2) + geom_point() + 
  labs(y = "Active Cases", title = "Figure 9. Total USA Covid-19 active cases each month") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-16-1.png)<!-- -->

``` r
lineplot4 <- ggplot(data = overall, aes(x = Month, y = Death, group = 1))
lineplot4 + geom_line(lwd = 2) + geom_point() + 
  labs(y = "Total Deaths", title = "Figure 10. Total USA Covid-19 deaths each month") + 
  theme(axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        title = element_text(size = 13))
```

![](/images/unnamed-chunk-16-2.png)<!-- -->
