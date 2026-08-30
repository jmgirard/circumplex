# Using Circumplex Instruments

``` r

library(circumplex)
```

## 1. Overview of Instrument-related Functions

Although the circumplex package is capable of analyzing and visualizing
data in a “source-agnostic” manner (i.e., without knowing what the
numbers correspond to), it can be helpful to both the user and the
package to have more contextual information about which
instrument/questionnaire the data come from. For example, knowing the
specific instrument used can enable the package to automatically score
item-level responses and standardize these scores using normative data.
Furthermore, a centralized repository of information about circumplex
instruments would provide a convenient and accessible way for users to
discover and begin using new instruments. To address these needs, a
suite of new instrument-related functions (and underlying data to power
them) was added to the circumplex package in version 0.2.0.

The first part of this vignette will discuss how to preview the
instruments currently available in the circumplex package, how to load
information about a specific instrument for use in analysis, and how to
extract general and specific information about that instrument. The
following functions will be discussed:
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md),
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`scales()`](http://circumplex.jmgirard.com/reference/scales.md),
[`items()`](http://circumplex.jmgirard.com/reference/items.md),
[`anchors()`](http://circumplex.jmgirard.com/reference/anchors.md),
[`norms()`](http://circumplex.jmgirard.com/reference/norms.md), and
[`View()`](https://rdrr.io/r/utils/View.html).

The second part of this vignette will discuss how to use the information
about an instrument to transform and summarize circumplex data. It will
demonstrate how to ipsatize item-level responses (i.e., apply deviation
scoring across variables), how to calculate scale scores from item-level
responses (with or without imputing/prorating missing values), and how
to standardize scale scores using normative/comparison data. The
following functions will be discussed:
[`ipsatize()`](http://circumplex.jmgirard.com/reference/ipsatize.md),
[`score()`](http://circumplex.jmgirard.com/reference/score.md), and
[`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md).

## 2. Loading and Examining Instrument Objects

### Previewing the available instruments

Our goal is to eventually include all circumplex instruments in this
package. However, due to time-constraints and copyright issues, this
goal may be delayed or even impossible to fully realize. However, you
can always preview the list of currently available instruments using the
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md)
function. This function will print the abbreviation, name, and (in
parentheses) the “code” for each available instrument. We will return to
the code in the next section.

``` r

instruments()
#> The circumplex package currently includes 15 instruments:
#>  1. CAIS: Child and Adolescent Interpersonal Survey (cais)
#>  2. CSIE: Circumplex Scales of Interpersonal Efficacy (csie)
#>  3. CSIG: Circumplex Scales of Intergroup Goals (csig)
#>  4. CSIP: Circumplex Scales of Interpersonal Problems (csip)
#>  5. CSIV: Circumplex Scales of Interpersonal Values (csiv)
#>  6. IEI: Interpersonal Emotion Inventory (iei)
#>  7. IGI-CR: Interpersonal Goals Inventory for Children, Revised Version (igicr)
#>  8. IIP-32: Inventory of Interpersonal Problems, Brief Version (iip32)
#>  9. IIP-64: Inventory of Interpersonal Problems (iip64)
#>  10. IIP-SC: Inventory of Interpersonal Problems Short Circumplex (iipsc)
#>  11. IIS-32: Inventory of Interpersonal Strengths, Brief Version (iis32)
#>  12. IIS-64: Inventory of Interpersonal Strengths (iis64)
#>  13. IIT-C: Inventory of Influence Tactics Circumplex (iitc)
#>  14. IPIP-IPC: IPIP Interpersonal Circumplex (ipipipc)
#>  15. ISC: Interpersonal Sensitivities Circumplex (isc)
```

If there are additional circumplex instruments you would like to have
added to the package (especially if you are the
author/copyright-holder), please [contact me](mailto:me@jmgirard.com).
Note that some information (e.g., item text and normative data) may not
be available for all instruments due to copyright issues. However, many
instruments are released as open-access and have full item text and
normative data included in the package.

### Loading a specific instrument

Instrument information is loaded into R’s memory when the circumplex
package is loaded. We can just refer to each by “code,” such as the
`csip` code for the Circumplex Scales of Interpersonal Problems. The
code is an object in R’s environment, so do not put it in quotes.

``` r

csip
#> CSIP: Circumplex Scales of Interpersonal Problems
#> 64 items, 8 scales, 1 normative data sets
#> Boudreaux, Ozer, Oltmanns, & Wright (2018)
#> <https://doi.org/10.1037/pas0000505>
```

### Examining an instrument in-depth

To examine the information available about a loaded instrument, there
are several options. To print a long list of formatted information about
the instrument, use the
[`summary()`](https://rdrr.io/r/base/summary.html) function. This will
return information about the instrument’s scales, rating scale anchors,
items, and normative data set(s). The summary of each instrument is also
available from the package
[reference](https://circumplex.jmgirard.com/reference/index.html#section-instrument-data)
page.

``` r

summary(ipipipc)
#> IPIP-IPC: IPIP Interpersonal Circumplex
#> 32 items, 8 scales, 1 normative data sets
#> Markey & Markey (2009)
#> <https://doi.org/10.1177/1073191109340382>
#> 
#> The IPIP-IPC contains 8 circumplex scales.
#> PA: Assured-Dominant (90 degrees)
#> BC: Arrogant-Calculating (135 degrees)
#> DE: Cold-Hearted (180 degrees)
#> FG: Aloof-Introverted (225 degrees)
#> HI: Unassured-Submissive (270 degrees)
#> JK: Unassuming-Ingenuous (315 degrees)
#> LM: Warm-Agreeable (360 degrees)
#> NO: Gregarious-Extraverted (45 degrees)
#> 
#> The IPIP-IPC is rated using the following 5-point scale.
#> 1. Very Inaccurate
#> 2. Moderately Inaccurate
#> 3. Neither Inaccurate nor Accurate
#> 4. Moderately Accurate
#> 5. Very Accurate
#> 
#> The IPIP-IPC contains 32 items (open-access):
#> 1. Am quiet around strangers
#> 2. Speak softly
#> 3. Tolerate a lot from others
#> 4. Am interested in people
#> 5. Feel comfortable around people
#> 6. Demand to be the center of interest
#> 7. Cut others to pieces
#> 8. Believe people should fend for themselves
#> 9. Am a very private person
#> 10. Let others finish what they are saying
#> 11. Take things as they come
#> 12. Reassure others
#> 13. Start conversations
#> 14. Do most of the talking
#> 15. Contradict others
#> 16. Don't fall for sob stories
#> 17. Don't talk a lot
#> 18. Seldom toot my own horn
#> 19. Think of others first
#> 20. Inquire about others' well-being
#> 21. Talk to a lot of different people at parties
#> 22. Speak loudly
#> 23. Snap at people
#> 24. Don't put a lot of thought into things
#> 25. Have little to say
#> 26. Dislike being the center of attention
#> 27. Seldom stretch the truth
#> 28. Get along well with others
#> 29. Love large parties
#> 30. Demand attention
#> 31. Have a sharp tongue
#> 32. Am not interested in other people's problems
#> 
#> The IPIP-IPC currently has 1 normative data set(s):
#> 1. 274 American college students
#> Reference kind: no identified source
#> Norms source unconfirmed; instrument published as Markey & Markey (2009)
#> <https://doi.org/10.1177/1073191109340382>
```

Specific subsections of this output can be returned individually using
the [`scales()`](http://circumplex.jmgirard.com/reference/scales.md),
[`anchors()`](http://circumplex.jmgirard.com/reference/anchors.md),
[`items()`](http://circumplex.jmgirard.com/reference/items.md), and
[`norms()`](http://circumplex.jmgirard.com/reference/norms.md)
functions. These functions are especially useful when you need to know a
specific bit of information about an instrument and don’t want the
console to be flooded with unneeded information.

``` r

anchors(ipipipc)
#> The IPIP-IPC is rated using the following 5-point scale.
#> 1. Very Inaccurate
#> 2. Moderately Inaccurate
#> 3. Neither Inaccurate nor Accurate
#> 4. Moderately Accurate
#> 5. Very Accurate
```

``` r

norms(ipipipc)
#> The IPIP-IPC currently has 1 normative data set(s):
#> 1. 274 American college students
#> Reference kind: no identified source
#> Norms source unconfirmed; instrument published as Markey & Markey (2009)
#> <https://doi.org/10.1177/1073191109340382>
```

Some of these functions also have additional arguments to customize
their output. For instance, the
[`scales()`](http://circumplex.jmgirard.com/reference/scales.md)
function has the `items` argument, which will display the items for each
scale when set to `TRUE`.

``` r

scales(ipipipc, items = TRUE)
#> The IPIP-IPC contains 8 circumplex scales.
#> PA: Assured-Dominant (90 degrees)
#>     6. Demand to be the center of interest
#>     14. Do most of the talking
#>     22. Speak loudly
#>     30. Demand attention
#> BC: Arrogant-Calculating (135 degrees)
#>     7. Cut others to pieces
#>     15. Contradict others
#>     23. Snap at people
#>     31. Have a sharp tongue
#> DE: Cold-Hearted (180 degrees)
#>     8. Believe people should fend for themselves
#>     16. Don't fall for sob stories
#>     24. Don't put a lot of thought into things
#>     32. Am not interested in other people's problems
#> FG: Aloof-Introverted (225 degrees)
#>     1. Am quiet around strangers
#>     9. Am a very private person
#>     17. Don't talk a lot
#>     25. Have little to say
#> HI: Unassured-Submissive (270 degrees)
#>     2. Speak softly
#>     10. Let others finish what they are saying
#>     18. Seldom toot my own horn
#>     26. Dislike being the center of attention
#> JK: Unassuming-Ingenuous (315 degrees)
#>     3. Tolerate a lot from others
#>     11. Take things as they come
#>     19. Think of others first
#>     27. Seldom stretch the truth
#> LM: Warm-Agreeable (360 degrees)
#>     4. Am interested in people
#>     12. Reassure others
#>     20. Inquire about others' well-being
#>     28. Get along well with others
#> NO: Gregarious-Extraverted (45 degrees)
#>     5. Feel comfortable around people
#>     13. Start conversations
#>     21. Talk to a lot of different people at parties
#>     29. Love large parties
```

## 3. Instrument-related Tidying Functions

It is a good idea in practice to digitize and save each participant’s
response to each item on an instrument, rather than just their scores on
each scale. Having access to item-level data will make it easier to spot
and correct mistakes, will enable more advanced analysis of missing
data, and will enable latent variable models that account for
measurement error (e.g., structural equation modeling). Furthermore, the
functions described below will make it easy to transform and summarize
such item-level data into scale scores.

First, however, we need to make sure the item-level data is in the
expected format. Your data should be stored in a data frame where each
row corresponds to one observation (e.g., participant, organization, or
timepoint) and each column corresponds to one variable describing these
observations (e.g., item responses, demographic characteristics, scale
scores). The [tidyverse](https://tidyverse.org) packages provide
excellent tools for getting your data into this format from a variety of
different file types and formats.

For the purpose of illustration, we will work with a small-scale data
set, which includes item-level responses to the Inventory of
Interpersonal Problems, Short Circumplex (IIP-SC) for just 10
participants. As will become important later on, this data set contains
a small amount of missing values (represented as `NA`). This data set is
included as part of the circumplex package and can be previewed as
follows:

``` r

raw_iipsc
#>    IIP01 IIP02 IIP03 IIP04 IIP05 IIP06 IIP07 IIP08 IIP09 IIP10 IIP11 IIP12
#> 1      0     0     0     0     1     0     1     0     2     1     0     0
#> 2      1     1     0     0     3     2     2     1     0     1     0     1
#> 3      1     0     1     0     1     1     1     3     0     1     0     0
#> 4      3     2     3    NA     2     3     2     3     2     3     2     4
#> 5      0     0     0     1     0     0     1     1     0     1     0     2
#> 6      0     0     0     0     0     0     1     1     0     0     0     0
#> 7      1     0     0     0     2     1     1     0     1     0     0     0
#> 8      1     0     1     0     1     1     2     1     1     0     0     0
#> 9      0     0     2     2     0     1     3     0     1     0     1     1
#> 10     0     0     0     0     0     0     2     0     0     0     0     0
#>    IIP13 IIP14 IIP15 IIP16 IIP17 IIP18 IIP19 IIP20 IIP21 IIP22 IIP23 IIP24
#> 1      0     1     4     3     2     4     2     0     1     0     0     0
#> 2      4     3     3     1     0     0     1     0     1     2     0     0
#> 3      2     3     3     2     2     1     1     0     3     2     3     1
#> 4      2     1     2     3     1     2     2     1     3     2     3     2
#> 5      1     1     3     1     0     1     0     1     1     0     1     1
#> 6      0     0     2     1     1     0     0     0     0     0     1     1
#> 7      1     1     1     0     1     0     0     0     0     1     1     1
#> 8      1    NA     2     1     1     0     1     0     0     0     1     1
#> 9      0     2     2     2     1     2     2     0     0     0     3     0
#> 10     0     2     2     1     0     0     0     0     0     0     0     0
#>    IIP25 IIP26 IIP27 IIP28 IIP29 IIP30 IIP31 IIP32
#> 1      3     3     3     0     0     0     1     0
#> 2      0     0     0     1     0     0     0     2
#> 3      1     1     1     0     3     2     3     2
#> 4      1     2     3     2     3     2     3     2
#> 5      2     1     0     0     0     0     0     0
#> 6      0     0     0     0     0     0     0     1
#> 7      1     0     0     0     1     1     0     0
#> 8      1     1     1     0     0     1     2     1
#> 9      1     0     1     0     0     1     3     0
#> 10     0     0     0     0     0     0     0     0
```

### Ipsatizing item-level data

For some forms of circumplex data analysis (e.g., analysis of circumplex
fit) but not others (e.g., structural summary method), it can be helpful
to transform item-level responses by subtracting each participant’s mean
across all items from his or her response on each item. This practice is
called “ipsatizing” or, more precisely, deviation scoring across
variables. This practice will attenuate the general factor across all
items and recasts the item-level responses as deviations from one’s own
mean rather than absolute responses. To perform ipsatizing and create a
new set of ipsatized responses to each item, use the
[`ipsatize()`](http://circumplex.jmgirard.com/reference/ipsatize.md)
function.

``` r

ips_iipsc <- ipsatize(data = raw_iipsc, items = 1:32, append = FALSE)
print(ips_iipsc)
#>       IIP01_i    IIP02_i    IIP03_i    IIP04_i    IIP05_i    IIP06_i    IIP07_i
#> 1  -1.0000000 -1.0000000 -1.0000000 -1.0000000  0.0000000 -1.0000000  0.0000000
#> 2   0.0625000  0.0625000 -0.9375000 -0.9375000  2.0625000  1.0625000  1.0625000
#> 3  -0.4062500 -1.4062500 -0.4062500 -1.4062500 -0.4062500 -0.4062500 -0.4062500
#> 4   0.7096774 -0.2903226  0.7096774         NA -0.2903226  0.7096774 -0.2903226
#> 5  -0.6250000 -0.6250000 -0.6250000  0.3750000 -0.6250000 -0.6250000  0.3750000
#> 6  -0.2812500 -0.2812500 -0.2812500 -0.2812500 -0.2812500 -0.2812500  0.7187500
#> 7   0.5000000 -0.5000000 -0.5000000 -0.5000000  1.5000000  0.5000000  0.5000000
#> 8   0.2580645 -0.7419355  0.2580645 -0.7419355  0.2580645  0.2580645  1.2580645
#> 9  -0.9687500 -0.9687500  1.0312500  1.0312500 -0.9687500  0.0312500  2.0312500
#> 10 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500  1.7812500
#>       IIP08_i    IIP09_i    IIP10_i    IIP11_i    IIP12_i    IIP13_i   IIP14_i
#> 1  -1.0000000  1.0000000  0.0000000 -1.0000000 -1.0000000 -1.0000000  0.000000
#> 2   0.0625000 -0.9375000  0.0625000 -0.9375000  0.0625000  3.0625000  2.062500
#> 3   1.5937500 -1.4062500 -0.4062500 -1.4062500 -1.4062500  0.5937500  1.593750
#> 4   0.7096774 -0.2903226  0.7096774 -0.2903226  1.7096774 -0.2903226 -1.290323
#> 5   0.3750000 -0.6250000  0.3750000 -0.6250000  1.3750000  0.3750000  0.375000
#> 6   0.7187500 -0.2812500 -0.2812500 -0.2812500 -0.2812500 -0.2812500 -0.281250
#> 7  -0.5000000  0.5000000 -0.5000000 -0.5000000 -0.5000000  0.5000000  0.500000
#> 8   0.2580645  0.2580645 -0.7419355 -0.7419355 -0.7419355  0.2580645        NA
#> 9  -0.9687500  0.0312500 -0.9687500  0.0312500  0.0312500 -0.9687500  1.031250
#> 10 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500  1.781250
#>       IIP15_i    IIP16_i    IIP17_i    IIP18_i    IIP19_i    IIP20_i    IIP21_i
#> 1   3.0000000  2.0000000  1.0000000  3.0000000  1.0000000 -1.0000000  0.0000000
#> 2   2.0625000  0.0625000 -0.9375000 -0.9375000  0.0625000 -0.9375000  0.0625000
#> 3   1.5937500  0.5937500  0.5937500 -0.4062500 -0.4062500 -1.4062500  1.5937500
#> 4  -0.2903226  0.7096774 -1.2903226 -0.2903226 -0.2903226 -1.2903226  0.7096774
#> 5   2.3750000  0.3750000 -0.6250000  0.3750000 -0.6250000  0.3750000  0.3750000
#> 6   1.7187500  0.7187500  0.7187500 -0.2812500 -0.2812500 -0.2812500 -0.2812500
#> 7   0.5000000 -0.5000000  0.5000000 -0.5000000 -0.5000000 -0.5000000 -0.5000000
#> 8   1.2580645  0.2580645  0.2580645 -0.7419355  0.2580645 -0.7419355 -0.7419355
#> 9   1.0312500  1.0312500  0.0312500  1.0312500  1.0312500 -0.9687500 -0.9687500
#> 10  1.7812500  0.7812500 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500
#>       IIP22_i    IIP23_i    IIP24_i    IIP25_i    IIP26_i    IIP27_i    IIP28_i
#> 1  -1.0000000 -1.0000000 -1.0000000  2.0000000  2.0000000  2.0000000 -1.0000000
#> 2   1.0625000 -0.9375000 -0.9375000 -0.9375000 -0.9375000 -0.9375000  0.0625000
#> 3   0.5937500  1.5937500 -0.4062500 -0.4062500 -0.4062500 -0.4062500 -1.4062500
#> 4  -0.2903226  0.7096774 -0.2903226 -1.2903226 -0.2903226  0.7096774 -0.2903226
#> 5  -0.6250000  0.3750000  0.3750000  1.3750000  0.3750000 -0.6250000 -0.6250000
#> 6  -0.2812500  0.7187500  0.7187500 -0.2812500 -0.2812500 -0.2812500 -0.2812500
#> 7   0.5000000  0.5000000  0.5000000  0.5000000 -0.5000000 -0.5000000 -0.5000000
#> 8  -0.7419355  0.2580645  0.2580645  0.2580645  0.2580645  0.2580645 -0.7419355
#> 9  -0.9687500  2.0312500 -0.9687500  0.0312500 -0.9687500  0.0312500 -0.9687500
#> 10 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500 -0.2187500
#>       IIP29_i    IIP30_i    IIP31_i    IIP32_i
#> 1  -1.0000000 -1.0000000  0.0000000 -1.0000000
#> 2  -0.9375000 -0.9375000 -0.9375000  1.0625000
#> 3   1.5937500  0.5937500  1.5937500  0.5937500
#> 4   0.7096774 -0.2903226  0.7096774 -0.2903226
#> 5  -0.6250000 -0.6250000 -0.6250000 -0.6250000
#> 6  -0.2812500 -0.2812500 -0.2812500  0.7187500
#> 7   0.5000000  0.5000000 -0.5000000 -0.5000000
#> 8  -0.7419355  0.2580645  1.2580645  0.2580645
#> 9  -0.9687500  0.0312500  2.0312500 -0.9687500
#> 10 -0.2187500 -0.2187500 -0.2187500 -0.2187500
```

Above, we told the function to take all the variables from column 1 to
column 32 and calculate new ipsatized variables ending in `_i` (although
different `prefix`es and `suffix`es can be used). By default, the mean
for each observation/row is calculated after ignoring any missing
values, but we could have changed this by adding `na.rm = FALSE`. By
setting `append` to FALSE, only the ipsatized versions are returned, but
if we changed this to TRUE then the original variables would have been
included as well.

We can check that the ipsatization was successful by calculating the
mean of each row (i.e., each participant’s mean response) in the
original and ipsatized data frames. We do this below using the
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) function; we also
apply the [`round()`](https://rdrr.io/r/base/Round.html) function to
make the results fit on one row. As expected, we find below that the
mean of each participant is zero in the ipsatized data frame but not in
the original.

``` r

round(rowMeans(raw_iipsc, na.rm = TRUE), 2)
#>  [1] 1.00 0.94 1.41 2.29 0.62 0.28 0.50 0.74 0.97 0.22
round(rowMeans(ips_iipsc, na.rm = TRUE), 2)
#>  [1] 0 0 0 0 0 0 0 0 0 0
```

### Scoring item-level data

For many forms of circumplex data analysis (e.g., structural summary
method), it can be very helpful to summarize item-level responses by
calculating scale scores. This is typically done by averaging a set of
items that all measure the same underlying construct (e.g., location in
the circumplex model). For example, the IIP-SC has 32 items in total
that measure 8 scales representing octants of the interpersonal
circumplex model. Thus, a participant’s score on each scale is
calculated as the arithmetic mean of his or her responses to four
specific items. Using the aggregate of multiple similar items produces
scale scores with higher reliability than would be achieved by using
only a single item per scale.

``` r

scales(iipsc)
#> The IIP-SC contains 8 circumplex scales.
#> PA: Domineering (90 degrees)
#> BC: Vindictive (135 degrees)
#> DE: Cold (180 degrees)
#> FG: Socially Avoidant (225 degrees)
#> HI: Nonassertive (270 degrees)
#> JK: Exploitable (315 degrees)
#> LM: Overly Nurturant (360 degrees)
#> NO: Intrusive (45 degrees)
```

Although calculating the arithmetic mean of a handful of items is not
terribly difficult mathematically, doing so manually (e.g., by hand)
across multiple scales and multiple participants can be tedious and
error-prone. To address these issues, the circumplex package offers the
[`score()`](http://circumplex.jmgirard.com/reference/score.md) function,
which automatically calculates scale scores from item-level data.

To demonstrate, let’s return to the `raw_iipsc` data set. We need to
give the [`score()`](http://circumplex.jmgirard.com/reference/score.md)
function a data frame containing the item-level data (i.e., the data
set), a list of variables from that data frame that contain the
item-level responses to be scored, and an instrument object containing
instructions on how to score the data. In order for scoring to work
properly, **the list of items must be in ascending order from the first
to the last item and the ordering of the items must be the same as that
assumed by the package**. Be sure to check your item numbers against
those displayed by the
[`items()`](http://circumplex.jmgirard.com/reference/items.md) function,
especially if you shuffle your items.

``` r

scale_scores <- score(
  data = raw_iipsc,
  items = 1:32,
  instrument = iipsc,
  append = FALSE
)
print(scale_scores)
#>      PA   BC   DE       FG   HI        JK   LM   NO
#> 1  1.75 2.00 1.25 0.000000 0.50 0.2500000 1.50 0.75
#> 2  0.25 0.50 0.25 0.500000 2.00 1.7500000 1.25 1.00
#> 3  1.00 0.75 0.75 0.000000 2.25 2.0000000 2.50 2.00
#> 4  1.75 2.25 2.50 2.333333 2.50 2.0000000 2.50 2.50
#> 5  0.50 0.75 0.00 1.000000 0.50 0.2500000 1.25 0.75
#> 6  0.25 0.00 0.00 0.000000 0.00 0.0000000 1.00 1.00
#> 7  1.00 0.00 0.00 0.000000 1.00 1.0000000 0.75 0.25
#> 8  1.00 0.25 0.75 0.000000 0.50 0.6666667 1.75 1.00
#> 9  0.75 0.50 1.50 0.750000 0.00 1.0000000 2.75 0.50
#> 10 0.00 0.00 0.00 0.000000 0.00 0.5000000 1.00 0.25
```

Because we set `append` to FALSE, the `scale_scores` data frame contains
only the scale score variables. These were named using the scale
abbreviations shown by the
[`scales()`](http://circumplex.jmgirard.com/reference/scales.md)
function (i.e., two-letter abbreviations from PA to NO). You can
customize the naming of these variables by using the `prefix` and
`suffix` arguments (e.g., to make them IIP_PA to IIP_NO).

Note that the `na.rm` argument for the
[`score()`](http://circumplex.jmgirard.com/reference/score.md) function
defaulted to `TRUE`, which means that missing values were ignored in
calculating the scale scores. This practice is common in the literature,
but is technically a form of single imputation and thus can produce
biased results when data are not missing completely at random (MCAR).
Please examine and report the amount and patterns of missingness in your
data.

### Standardizing scale-level data

Finally, it can often be helpful to transform scale-level data through
reference to a *reference sample*: a specific group of people whose
scores on the same instrument the package ships as a table of means and
standard deviations. This is often called “norm standardizing” and
involves subtracting the reference sample’s mean score on a scale from
each participant’s score on that scale and then dividing this difference
by the reference sample’s standard deviation. This rescales the scale
scores to be in standard deviation units and to describe the magnitude
of each participant’s difference from the average of that particular
sample.

For many circumplex instruments, the data needed to perform
standardization is included in its instrument object. Some instruments
have more than one reference sample — for example, separate samples for
men and women, or for children and adults. In selecting a reference
sample, the first question to ask is which group your participants are
most sensibly described relative to; the size of the sample is a
secondary consideration.

Before choosing, it is worth knowing what these reference samples
generally are.

``` r

inst <- Filter(
  function(x) inherits(x, "circumplex_instrument"),
  mget(
    utils::data(package = "circumplex")$results[, "Item"],
    envir = as.environment("package:circumplex"),
    ifnotfound = list(NULL)
  )
)
samples <- do.call(rbind, lapply(inst, function(x) x$Norms[[2]]))

n_instruments <- length(inst)
n_samples <- nrow(samples)
n_college <- sum(grepl("college|undergraduate", samples$Population))
n_small <- sum(samples$Size < 300)
n_standardization <- sum(samples$Kind == "standardization")
n_unsourced <- sum(samples$Kind == "unsourced")
```

The package ships 23 reference samples across 15 instruments. 10 of them
describe college or undergraduate students, and 6 have fewer than 300
participants. Most are the single study sample that was available to the
researchers who published the instrument — people recruited at one
institution, at one time — rather than a group assembled to represent
any wider population. The exception is the IIP-32 and IIP-64, whose 6
samples come from a national standardization study designed for exactly
that purpose. At the other end, 2 of the tables are published in no
source that has been identified, and scores standardized against them
rest on unverified numbers. You do not have to work any of this out from
the descriptions: every sample carries a `Kind` — `standardization`,
`published`, or `unsourced` — which is where the two counts just given
come from, and which
[`norms()`](http://circumplex.jmgirard.com/reference/norms.md) prints
and
[`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
names in the message it prints unless you silence it. A reference sample
is therefore best read as a concrete, described group of people to
compare against, and the `Population` column printed below names the
group each sample was drawn from rather than a population the sample
stands in for.

To demonstrate, let’s examine the reference samples available for the
IIP-SC. Below we see two options: a sample of American college students
and a sample of American psychiatric outpatients. They differ greatly in
size, but what makes one or the other the right choice is which group
your participants resemble.

``` r

norms(iipsc)
#> The IIP-SC currently has 2 normative data set(s):
#> 1. 872 American college students
#> Reference kind: identified published source
#> Hopwood, Pincus, DeMoor, & Koonce (2008)
#> <https://doi.org/10.1080/00223890802388665>
#> 2. 106 American psychiatric outpatients
#> Reference kind: identified published source
#> Soldz, Budman, Demby, & Merry (1995)
#> <https://doi.org/10.1177/1073191195002001006>
```

Assuming our example data also come from a non-psychiatric community
sample of mostly college students, the first reference sample is the
better choice — because it describes a similar group of people, not
because it is larger. Which sample you choose matters a great deal more
than how precisely that sample’s mean and standard deviation are
estimated: switching to a different reference sample typically shifts
standardized scores by several times the uncertainty that even the
smallest shipped sample contributes. If your participants were
psychiatric outpatients, the second sample would be the appropriate
reference despite its size.

To transform the scale scores we calculated during the last section, we
can call the
[`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
function and give it the `scale_scores` object we created above. We will
save the output of this function to a data frame named `z_scales` to
reflect the idea that standardized scores are often called “z-scores.”

``` r

z_scales <- norm_standardize(
  data = scale_scores,
  scales = 1:8,
  instrument = iipsc,
  sample = 1,
  append = FALSE
)
#> Standardized against IIP-SC normative sample 1: N = 872, American college students. Reference kind: identified published source. 1 other sample is available; see norms().
print(z_scales)
#>           PA_z       BC_z       DE_z        FG_z       HI_z       JK_z
#> 1   1.50000000  1.7500000  0.4093567 -1.10554090 -1.0054645 -1.3313783
#> 2  -0.77272727 -0.4239130 -0.7602339 -0.57783641  0.6338798  0.4281525
#> 3   0.36363636 -0.0615942 -0.1754386 -1.10554090  0.9071038  0.7214076
#> 4   1.50000000  2.1123188  1.8713450  1.35708004  1.1803279  0.7214076
#> 5  -0.39393939 -0.0615942 -1.0526316 -0.05013193 -1.0054645 -1.3313783
#> 6  -0.77272727 -1.1485507 -1.0526316 -1.10554090 -1.5519126 -1.6246334
#> 7   0.36363636 -1.1485507 -1.0526316 -1.10554090 -0.4590164 -0.4516129
#> 8   0.36363636 -0.7862319 -0.1754386 -1.10554090 -1.0054645 -0.8426197
#> 9  -0.01515152 -0.4239130  0.7017544 -0.31398417 -1.5519126 -0.4516129
#> 10 -1.15151515 -1.1485507 -1.0526316 -1.10554090 -1.5519126 -1.0381232
#>           LM_z     NO_z
#> 1   0.04242424 -0.34375
#> 2  -0.26060606 -0.03125
#> 3   1.25454545  1.21875
#> 4   1.25454545  1.84375
#> 5  -0.26060606 -0.34375
#> 6  -0.56363636 -0.03125
#> 7  -0.86666667 -0.96875
#> 8   0.34545455 -0.03125
#> 9   1.55757576 -0.65625
#> 10 -0.56363636 -0.96875
```

Again, because we set `append` to FALSE, the output contains only the
norm standardized scale-level variables. The new variables are named the
same as the scale score variables except with a configurable `prefix`
and `suffix` (by default, they are given only a suffix of `_z`). These
variables are the ones we are most likely to use in subsequent analyses
(e.g., the structural summary method).

## 4. Wrap-up

In this vignette, we learned how to preview the instruments available in
the circumplex package, load and examine the information contained in
one of these instrument objects, ipsatize item-level data, calculate
scale scores from item-level data, and standardize those scale scores
using normative data included in the package. We are now in an excellent
position to discover and implement new circumplex instruments. Later
vignettes describe analyses and visualizations that make use of the data
collected using these tools.

Special thanks to the authors and publishers who granted permission to
include information about their instruments in this package: Chloe
Bliton, Michael Boudreaux, Robert Hatcher, Christopher Hopwood, Leonard
Horowitz, Kenneth Locke, Patrick Markey, Aaron Pincus, Elisa Trucco, and
MindGarden Inc.
