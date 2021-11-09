

## Introduction

This package is the outcome of the Mastering Software Development in R Capstone.

### Description

The package is tailored to work with the [NOAA][noaa_website] (National Oceanic Atmospheric Administration) [Earthquake database][noaa_earthquake].

[noaa_website]: https://www.ngdc.noaa.gov
[noaa_earthquake]: https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1

This database has 6,086 observations and 47 features (database downloaded in 20/feb/2019), which 4,283 observations are about earthquake and 1,803 with `FLAG_TSUNAMI` as true.

From this 4,283 observations, there are 27 with negative `YEAR` and 4,256 with positives values. Finally, from this last subset 1,305 observations have no `EQ_PRIMARY` (Magnitude in [Richter Scale][ritcher_scale]), which means they are recorded as `NA`, so there are only 2,951 valid observations.

The following 6 functions are included in this package:
<ol>
  <li>`eq_clean_data()`
  <li>`eq_location_clean()`
  <li>`geom_timeline()`
  <li>`geom_timeline_label()`
  <li>`eq_map()`
  <li>`eq_create_label()`
</ol>

