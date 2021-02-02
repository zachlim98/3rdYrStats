Introduction
============

With Singapore now in recovery from the Covid-19 pandemic, the
government has began to allow people to meet up in groups of 8. My
church group (of 37 people) wanted to meet up for dinner. I hence
decided to use R to write some code that would allow us to organise the
dinner in the best way possible! (*fictitious names have been used for
privacy*)

Sorting Requirements
====================

1.  People stay all around Singapore
2.  There are 4 dinner locations
3.  People studied in different regions around the world.
4.  We needed to sort people first by *region* and then by *dinner
    location*.

This was looking to be a fun challenge!

Brain-storming for ideas
========================

With these requirements in mind, I had to start thinking about how I
would accomplish this. In order to ensure that people went to the most
convenient location, the obvious metric we could use was the travel time
between their houses and the dinner location. In order to find that, I
used the `mapsapi` which allows us to access the Directions API within
the Google Maps API suite. Below is an example of how I used that!
(note: MRT stands for Mass Rapid Transit is Singapore’s subway)

``` r
library(mapsapi)
```

    ## Warning: package 'mapsapi' was built under R version 4.0.3

``` r
key = "AIzaSyDmTh_LFt4SqVslqiwo3pFw8tMaAta6p0k"

doc = mp_directions(
  origin = "Woodlands MRT, Singapore",
  destination = "Holland Village MRT, Singapore",
  alternatives = FALSE,
  mode = "transit",
  key = key, 
  quiet = TRUE
)

r = mp_get_routes(doc)

time = r$duration_text
time
```

    ## [1] "55 mins"
