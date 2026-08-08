# Child and Adolescent Interpersonal Survey

Information about the Child and Adolescent Interpersonal Survey (CAIS).

## Usage

``` r
cais
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Sodano, S. M., & Tracey, T. J. G. (2006). Interpersonal traits in
childhood: Development of the Child and Adolescent Interpersonal Survey.
*Journal of Personality Assessment, 87*(3), 317–329.

[doi:10.1207/s15327752jpa8703_12](https://doi.org/10.1207/s15327752jpa8703_12)

## Note on the adult normative sample

The CAIS is rated on a 5-point scale, but three of the octant means the
source publishes for its adult sample (`sample = 2`) fall above 5: PA
5.19, LM 6.52, and NO 6.14. The package transcribes them faithfully, and
the source's own table is the origin of the discrepancy rather than the
transcription. Because a sample whose means lie outside the response
range cannot be on the same metric as the scores being standardized,
[`norm_standardize()`](http://circumplex.jmgirard.com/reference/norm_standardize.md)
refuses `sample = 2` rather than returning z-scores in an undefined
unit. The child sample (`sample = 1`) is unaffected and its means all
sit inside the response range. The discrepancy is under query with the
instrument's authors; this sample will be corrected or withdrawn once
that is resolved.

## Examples

``` r
summary(cais)
#> CAIS: Child and Adolescent Interpersonal Survey
#> 37 items, 8 scales, 2 normative data sets
#> Sodano & Tracey (2006)
#> <https://doi.org/10.1207/s15327752jpa8703_12>
#> 
#> The CAIS contains 8 circumplex scales.
#> PA: Assured-Dominant (90 degrees)
#> BC: Arrogant-Calculating (135 degrees)
#> DE: Cold-Hearted (180 degrees)
#> FG: Aloof-Introverted (225 degrees)
#> HI: Unassured-Submissive (270 degrees)
#> JK: Unassuming-Ingenuous (315 degrees)
#> LM: Warm-Agreeable (360 degrees)
#> NO: Gregarious-Extraverted (45 degrees)
#> 
#> The CAIS is rated using the following 5-point scale.
#> 1. Never
#> 2. A little
#> 3. Some
#> 4. A lot
#> 5. Always
#> 
#> The CAIS contains 37 items (open-access):
#> 1. I am tough
#> 2. I call people names
#> 3. I hurt people
#> 4. I am by myself a lot
#> 5. I am shy
#> 6. I am calm
#> 7. I am kind to others
#> 8. I am fun to be around
#> 9. I know a lot
#> 10. I like making trouble
#> 11. I make people cry
#> 12. I am alone
#> 13. I am sad
#> 14. I am quiet
#> 15. I try to help others feel better
#> 16. I am happy
#> 17. I think I can do a lot
#> 18. I trick people
#> 19. I am mean to others
#> 20. I am hard to get to know
#> 21. I know very little
#> 22. Tricking people is mean
#> 23. I am friendly
#> 24. I am giving
#> 25. I speak up for myself
#> 26. I tell people what to do
#> 27. I like it when others feel bad
#> 28. I play by myself
#> 29. I give in easily
#> 30. I help people
#> 31. I play with others
#> 32. I think I am right
#> 33. I am sneaky
#> 34. I am grumpy
#> 35. I am afraid
#> 36. I share
#> 37. I have a lot of friends
#> 
#> The CAIS currently has 2 normative data set(s):
#> 1. 204 American fourth and sixth graders (aged 9 to 13)
#> Sodano & Tracey (2006)
#> <https://doi.org/10.1207/s15327752jpa8703_12>
#> 2. 194 American college students (aged 17 to 50)
#> Sodano & Tracey (2006)
#> <https://doi.org/10.1207/s15327752jpa8703_12>
scales(cais, items = TRUE)
#> The CAIS contains 8 circumplex scales.
#> PA: Assured-Dominant (90 degrees)
#>     1. I am tough
#>     9. I know a lot
#>     17. I think I can do a lot
#>     25. I speak up for myself
#>     32. I think I am right
#> BC: Arrogant-Calculating (135 degrees)
#>     2. I call people names
#>     10. I like making trouble
#>     18. I trick people
#>     26. I tell people what to do
#>     33. I am sneaky
#> DE: Cold-Hearted (180 degrees)
#>     3. I hurt people
#>     11. I make people cry
#>     19. I am mean to others
#>     27. I like it when others feel bad
#>     34. I am grumpy
#> FG: Aloof-Introverted (225 degrees)
#>     4. I am by myself a lot
#>     12. I am alone
#>     20. I am hard to get to know
#>     28. I play by myself
#> HI: Unassured-Submissive (270 degrees)
#>     5. I am shy
#>     13. I am sad
#>     21. I know very little
#>     29. I give in easily
#>     35. I am afraid
#> JK: Unassuming-Ingenuous (315 degrees)
#>     6. I am calm
#>     14. I am quiet
#>     22. Tricking people is mean
#> LM: Warm-Agreeable (360 degrees)
#>     7. I am kind to others
#>     15. I try to help others feel better
#>     23. I am friendly
#>     30. I help people
#>     36. I share
#> NO: Gregarious-Extraverted (45 degrees)
#>     8. I am fun to be around
#>     16. I am happy
#>     24. I am giving
#>     31. I play with others
#>     37. I have a lot of friends
```
