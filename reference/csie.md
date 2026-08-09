# Circumplex Scales of Interpersonal Efficacy

Information about the Circumplex Scales of Interpersonal Efficacy
(CSIE).

## Usage

``` r
csie
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Locke, K. D., & Sadler, P. (2007). Self-efficacy, values, and
complementarity in dyadic interactions: Integrating interpersonal and
social-cognitive theory. *Personality and Social Psychology Bulletin,
33*(1), 94-109. The normative sample below is this article's Study 1
sample, but the octant means and standard deviations themselves are
published in the author's norms table, not in the article:
<https://kennethlocke.org/CSIE/CSIE_Norms.html>

<https://kennethlocke.org/CSIE/CSIE.html>

## Examples

``` r
summary(csie)
#> CSIE: Circumplex Scales of Interpersonal Efficacy
#> 32 items, 8 scales, 1 normative data sets
#> Locke & Sadler (2007)
#> <https://doi.org/10.1177/0146167206293375>
#> 
#> The CSIE contains 8 circumplex scales.
#> PA: +A (90 degrees)
#> BC: +A-C (135 degrees)
#> DE: -C (180 degrees)
#> FG: -A-C (225 degrees)
#> HI: -A (270 degrees)
#> JK: -A+C (315 degrees)
#> LM: +C (360 degrees)
#> NO: +A+C (45 degrees)
#> 
#> The CSIE is rated using the following 11-point scale.
#> 0. I am not at all confident that
#> 1. I am not at all confident that
#> 2. I am mildly confident that
#> 3. I am mildly confident that
#> 4. I am moderately confident that
#> 5. I am moderately confident that
#> 6. I am moderately confident that
#> 7. I am very confident that
#> 8. I am very confident that
#> 9. I am absolutely confident that
#> 10. I am absolutely confident that
#> 
#> The CSIE contains 32 items (open-access):
#> Prefix: When I am with others, ...
#> 1. I can express myself openly
#> 2. I can be tough
#> 3. I can follow the rules
#> 4. I can be assertive
#> 5. I can hide my thoughts and feelings
#> 6. I can fit in
#> 7. I can keep the upper hand
#> 8. I can avoid getting into arguments
#> 9. I can smooth over any difficulties
#> 10. I can be cold and unfriendly when I want to
#> 11. I can get along with them
#> 12. I can speak up when I have something to say
#> 13. I can be submissive
#> 14. I can understand their feelings
#> 15. I can win any arguments or competitions
#> 16. I can be a follower
#> 17. I can get them to listen to what I have to say
#> 18. I can get them to leave me alone
#> 19. I can be nice
#> 20. I can take charge
#> 21. I can disappear into the background when I want
#> 22. I can soothe hurt feelings
#> 23. I can be aggressive if I need to
#> 24. I can avoid making them angry
#> 25. I can be a leader
#> 26. I can be cruel when the situation calls for it
#> 27. I can be giving
#> 28. I can be forceful
#> 29. I can be quiet
#> 30. I can be helpful
#> 31. I can tell them when I am annoyed
#> 32. I can let others take charge
#> 
#> The CSIE currently has 1 normative data set(s):
#> 1. 367 American college students
#> Reference kind: identified published source
#> Locke & Sadler (2007)
#> <https://kennethlocke.org/CSIE/CSIE_Norms.html>
scales(csie, items = TRUE)
#> The CSIE contains 8 circumplex scales.
#> PA: +A (90 degrees)
#>     4. I can be assertive
#>     12. I can speak up when I have something to say
#>     20. I can take charge
#>     28. I can be forceful
#> BC: +A-C (135 degrees)
#>     7. I can keep the upper hand
#>     15. I can win any arguments or competitions
#>     23. I can be aggressive if I need to
#>     31. I can tell them when I am annoyed
#> DE: -C (180 degrees)
#>     2. I can be tough
#>     10. I can be cold and unfriendly when I want to
#>     18. I can get them to leave me alone
#>     26. I can be cruel when the situation calls for it
#> FG: -A-C (225 degrees)
#>     5. I can hide my thoughts and feelings
#>     13. I can be submissive
#>     21. I can disappear into the background when I want
#>     29. I can be quiet
#> HI: -A (270 degrees)
#>     8. I can avoid getting into arguments
#>     16. I can be a follower
#>     24. I can avoid making them angry
#>     32. I can let others take charge
#> JK: -A+C (315 degrees)
#>     3. I can follow the rules
#>     11. I can get along with them
#>     19. I can be nice
#>     27. I can be giving
#> LM: +C (360 degrees)
#>     6. I can fit in
#>     14. I can understand their feelings
#>     22. I can soothe hurt feelings
#>     30. I can be helpful
#> NO: +A+C (45 degrees)
#>     1. I can express myself openly
#>     9. I can smooth over any difficulties
#>     17. I can get them to listen to what I have to say
#>     25. I can be a leader
```
