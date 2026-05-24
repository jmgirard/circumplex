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
#> 1. 213 American fourth and sixth graders (aged 9 to 13)
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
#> BC: Arrogant-Calculating (135 degrees)
#>     2. I call people names
#>     10. I like making trouble
#>     18. I trick people
#>     26. I tell people what to do
#> DE: Cold-Hearted (180 degrees)
#>     3. I hurt people
#>     11. I make people cry
#>     19. I am mean to others
#>     27. I like it when others feel bad
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
#> JK: Unassuming-Ingenuous (315 degrees)
#>     6. I am calm
#>     14. I am quiet
#>     22. Tricking people is mean
#>     30. I help people
#> LM: Warm-Agreeable (360 degrees)
#>     7. I am kind to others
#>     15. I try to help others feel better
#>     23. I am friendly
#>     31. I play with others
#> NO: Gregarious-Extraverted (45 degrees)
#>     8. I am fun to be around
#>     16. I am happy
#>     24. I am giving
#>     32. I think I am right
```
