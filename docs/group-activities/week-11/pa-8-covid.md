# PA 8: COVID-19 Infections and California
Instructions

- [Goals for the Activity](#goals-for-the-activity)
- [Setup - United States COVID-19 Cases and
  Deaths](#setup---united-states-covid-19-cases-and-deaths)
- [California COVID-19 Cases](#california-covid-19-cases)
  - [California Monthly COVID-19 Cases from January 2020 - March
    2023](#california-monthly-covid-19-cases-from-january-2020---march-2023)
  - [What Did California COVID-19 Cases Look Like in
    2021](#what-did-california-covid-19-cases-look-like-in-2021)
  - [Create your own graph](#create-your-own-graph)

***This task is complex. It requires many different types of abilities.
Everyone will be good at some of these abilities but nobody will be good
at all of them. In order to produce the best product possible, you will
need to use the skills of each member of your group.***

<!-- The person who **had** the most relaxing Spring Break starts as the Developer (typing and listening to instructions from the Coder)!  -->

## Goals for the Activity

- Use the `dplyr` verbs to transform your data  
- Use other `tidyverse` functions to prepare and plot the data  
- Use `lubridate` to deal with dates

**THROUGHOUT THE Activity** be sure to follow the Style Guide by doing
the following:

- load the appropriate packages at the beginning of the Quarto
  document  
- use proper spacing  
- *add labels* to all code chunks  
- comment at least once in each code chunk to describe why you made your
  coding decisions  
- add appropriate labels to all graphic axes

## Setup - United States COVID-19 Cases and Deaths

Starting in January 2020, the New York Times started reporting on
COVID-19 infections in the United States and eventually created a
[Githhub Repository](https://github.com/nytimes/covid-19-data/) of the
data they used and reported on in their stories (a field called “Data
Journalism”). They ended their data collection in March 2023 and
switched to just using data from national reporting systems.

We will use their data to evaluate COVID-19 and how it varied across
different states. Here are the NY Times data on cumulative cases by date
and states (including territories).

``` r
# data comes from NY Times GitHub Repository - ends March 2023
# not evaluating this code chunk because we will clean the data and use the clean data set
cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

Note that both `cases` and `deaths` are cumulative by date and we will
want to observe just the unique cases per day so we can get various
estimates of totals across the states.

If we want to extract the daily cases we can use the following code
which will calculate the difference in (cumulative) cases for one day
minus the previous day using the `diff()` function from `base` R.

``` r
# calculates the unique new daily cases and deaths for each state
# eval is false since we created a new data set we will use going forward
cases |> 
  group_by(state) |> 
  arrange(state, date) |> 
  mutate(cases_daily = c(cases[1], diff(cases)),
         deaths_daily = c(deaths[1], diff(deaths))) |> 
  ungroup() |> 
  write_csv("covid_cases_us.csv")
```

Now that are data is in the format we need (sort of), we will start our
analysis.

## California COVID-19 Cases

First, we need to read in our clean data.

``` r
cases <- read_csv("covid_cases_us.csv")
```

### California Monthly COVID-19 Cases from January 2020 - March 2023

We want to create a graph that plots the number of cases per month in
California. We want the x-axis of the plot to have the month/year
(e.g. March 2021), and the number of cases on the y-axis. The
months/years should be in order chronologically.

Below is some code that can help you get started with the process of
working with the data. Use the functions from `lubridate` to help you
extract out the right information. You will also use functions learned
from other packages such as `dplyr`.

``` r
cases |> 
  filter(____________) |> 
  mutate(date = ___(date),
         month = ___(date, label = , abbr = ),
         year = ___(date)) |> 
  unite()
```

#### Canvas Quiz Question 1

**Which Month in California had the most COVID-19 cases between January
2020 and March 2023?**

> Insert Answer Here

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

### What Did California COVID-19 Cases Look Like in 2021

Create a graph that maps the number of cases in California in 2021 only.
Be sure to add all guidelines for creating good graphics.

#### Canvas Quiz Question 2

**Which month in 2021 had the fewest number of cases in California?**

> Insert Answer Here

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

### Create your own graph

Using the COVID-19 data, create your graph to answer you own research
question. You will submit that graph as png file in your Canvas quiz.
Include your research question as the subtitle of the graph.
