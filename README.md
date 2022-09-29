HW1_433
================
Grace Campbell
9/22/2022

``` r
library(dplyr)
library(nycflights13)
library(ggplot2)
```

``` r
head(flights)
```

    ## # A tibble: 6 × 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ## 1  2013     1     1      517            515         2      830            819
    ## 2  2013     1     1      533            529         4      850            830
    ## 3  2013     1     1      542            540         2      923            850
    ## 4  2013     1     1      544            545        -1     1004           1022
    ## 5  2013     1     1      554            600        -6      812            837
    ## 6  2013     1     1      554            558        -4      740            728
    ## # … with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

### QUESTION 1.)

- How many flights have a missing dep_time? What other variables are
  missing? What might these rows represent?

``` r
sum(is.na(flights$dep_time))
```

    ## [1] 8255

``` r
filter(flights, is.na(dep_time))
```

    ## # A tibble: 8,255 × 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1       NA           1630        NA       NA           1815
    ##  2  2013     1     1       NA           1935        NA       NA           2240
    ##  3  2013     1     1       NA           1500        NA       NA           1825
    ##  4  2013     1     1       NA            600        NA       NA            901
    ##  5  2013     1     2       NA           1540        NA       NA           1747
    ##  6  2013     1     2       NA           1620        NA       NA           1746
    ##  7  2013     1     2       NA           1355        NA       NA           1459
    ##  8  2013     1     2       NA           1420        NA       NA           1644
    ##  9  2013     1     2       NA           1321        NA       NA           1536
    ## 10  2013     1     2       NA           1545        NA       NA           1910
    ## # … with 8,245 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

- There are 8255 flights that have missing dep_time. The other variables
  which have missing data. Looking at the printed table above, we can
  see that dep_delay, arr_time, arr_delay, and air_time all have missing
  values within these rows. These rows could possibly represent
  cancelled flights as all of the information that would be from when
  the airplane is actually in the leaving/ in the air/ landing is
  missing.

#### QUESTION 2.)

- Currently dep_time and sched_dep_time are convenient to look at, but
  hard to compute with because they’re not really continuous numbers.
  Convert them to a more convenient representation of number of minutes
  since midnight.

``` r
flights2 <- mutate(flights, 
                   sched_min = (sched_dep_time%%100*60+sched_dep_time%%100)%%1440, 
                   actual_min = (dep_time%%100*60+dep_time%%100)%%1440) 
flights2 <- flights2 %>%
  select(dep_time, actual_min, sched_dep_time, sched_min)
flights2
```

    ## # A tibble: 336,776 × 4
    ##    dep_time actual_min sched_dep_time sched_min
    ##       <int>      <dbl>          <int>     <dbl>
    ##  1      517       1037            515       915
    ##  2      533        573            529       329
    ##  3      542       1122            540      1000
    ##  4      544       1244            545      1305
    ##  5      554        414            600         0
    ##  6      554        414            558       658
    ##  7      555        475            600         0
    ##  8      557        597            600         0
    ##  9      557        597            600         0
    ## 10      558        658            600         0
    ## # … with 336,766 more rows

#### QUESTION 3.)

- Look at the number of canceled flights per day. Is there a pattern? Is
  the proportion of canceled flights related to the average delay? Use
  multiple dyplr operations, all on one line, concluding with
  ggplot(aes(x= ,y=)) + geom_point()

``` r
cancelled <- flights %>%
  mutate(cancel = (is.na(dep_delay) | (is.na(arr_delay)))) %>%
  group_by(year, month, day) %>%
  summarise(cancel_n = sum(cancel), flights_n = n(), .groups = "keep") 
  ggplot(cancelled, aes(x=flights_n,y=cancel_n)) +
  geom_point() +
    xlab("Number of Flights") +
    ylab("Number of Cancelled Flights") +
    ggtitle("Cancelled Flight Pattern")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

- Looking at the graph above, we can see that as the number of flights
  per day increases, the number of cancelled flights also increases on
  average.

``` r
arrive <- flights %>%
  mutate(cancel = (is.na(dep_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarize(prop_cancelled = mean(cancel), avg_arr_delay = mean(arr_delay, na.rm=TRUE), .groups = "keep")
  ggplot(arrive, aes(x=avg_arr_delay,y=prop_cancelled)) +
  geom_point() +
    xlab("Average Arrival Delay") +
    ylab("Number of Cancelled Flights") +
    ggtitle("Cancelled Flight Pattern (Arrivals)")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
depart <- flights %>%
  mutate(cancel = (is.na(dep_delay) | (is.na(arr_delay)))) %>%
  group_by(year, month, day) %>%
  summarize(prop_cancelled = mean(cancel), avg_dep_delay = mean(dep_delay, na.rm=TRUE), .groups = "keep") 
  ggplot(depart, aes(x=avg_dep_delay,y=prop_cancelled)) +
  geom_point() +
    xlab("Average Departure Delay") +
    ylab("Number of Cancelled Flights") +
    ggtitle("Cancelled Flight Pattern (Departures)")
```

![](README_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

- Looking at the graphs above, we can see that the number of flights
  cancelled increases when arrival delay time increases, as well as when
  departure delay time increases.
