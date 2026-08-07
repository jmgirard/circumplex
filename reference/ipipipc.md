# IPIP Interpersonal Circumplex

Information about the IPIP Interpersonal Circumplex (IPIP-IPC).

## Usage

``` r
ipipipc
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Markey, P. M., & Markey, C. N. (2009). A brief assessment of the
interpersonal circumplex: The IPIP-IPC. *Assessment, 16*(4), 352-361.
This article defines the instrument and supplies its item-to-octant
assignment and response anchors (Appendix, p. 360); the only octant
means and standard deviations it reports are for a different sample
(Study 1 combined, p. 354).

Norms: source unconfirmed. The shipped octant means and standard
deviations are published in no source that has been identified. The
sample size of 274 is the article's Study 2 (p. 357), for which the
article reports no descriptive statistics. Treat them as unverified
until a source is established.

[doi:10.1177/1073191109340382](https://doi.org/10.1177/1073191109340382)

## Examples

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
#> Norms source unconfirmed; instrument published as Markey & Markey (2009)
#> <https://doi.org/10.1177/1073191109340382>
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
