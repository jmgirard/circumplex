# Circumplex Scales of Intergroup Goals

Information about the Circumplex Scales of Intergroup Goals (CSIG).

## Usage

``` r
csig
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Locke, K. D. (2014). Circumplex scales of intergroup goals: An
interpersonal circle model of goals for interactions between groups.
*Personality and Social Psychology Bulletin, 40*(4), 433-449.

<https://kennethlocke.org/CSIG/CSIG.html>

## Examples

``` r
summary(csig)
#> CSIG: Circumplex Scales of Intergroup Goals
#> 32 items, 8 scales, 1 normative data sets
#> Locke (2014)
#> <https://doi.org/10.1177/0146167213514280>
#> 
#> The CSIG contains 8 circumplex scales.
#> PA: Be authoritative (90 degrees)
#> BC: Be tough (135 degrees)
#> DE: Be self-protective (180 degrees)
#> FG: Be wary (225 degrees)
#> HI: Be conflict-avoidant (270 degrees)
#> JK: Be cooperative (315 degrees)
#> LM: Be understanding (360 degrees)
#> NO: Be respected (45 degrees)
#> 
#> The CSIG is rated using the following 5-point scale.
#> 0. It is not at all important that...
#> 1. It is somewhat important that...
#> 2. It is moderately important that...
#> 3. It is very important that...
#> 4. It is extremely important that...
#> 
#> The CSIG contains 32 items (open-access):
#> Prefix: In dealing with other groups, how important is it that we act or appear or are treated this way?
#> 1. We are friendly
#> 2. We are the winners in any argument or dispute
#> 3. They respect what we have to say
#> 4. We avoid conflict
#> 5. We show that we can be tough
#> 6. We appreciate what they have to offer
#> 7. We let them fend for themselves
#> 8. We are assertive
#> 9. We celebrate their achievements
#> 10. We do whatever is in our best interest
#> 11. We get the chance to express our views
#> 12. They not get angry with us
#> 13. We not appear vulnerable
#> 14. We understand their point of view
#> 15. They stay out of our business
#> 16. We appear confident
#> 17. They feel we are all on the same team
#> 18. We are better than them
#> 19. They listen to what we have to say
#> 20. We not get into arguments
#> 21. We are aggressive if necessary
#> 22. We show concern for their welfare
#> 23. We not trust them
#> 24. We are decisive
#> 25. We are cooperative
#> 26. We keep our guard up
#> 27. They see us as responsible
#> 28. We not make them angry
#> 29. We not show our weaknesses
#> 30. We are able to compromise
#> 31. We not get entangled in their affairs
#> 32. They see us as capable
#> 
#> The CSIG currently has 1 normative data set(s):
#> 1. 665 MTurkers from US, Canada, and India about interactions between nations
#> Reference kind: identified published source
#> Locke (2014)
#> <https://doi.org/10.1177/0146167213514280>
scales(csig, items = TRUE)
#> The CSIG contains 8 circumplex scales.
#> PA: Be authoritative (90 degrees)
#>     8. We are assertive
#>     16. We appear confident
#>     24. We are decisive
#>     32. They see us as capable
#> BC: Be tough (135 degrees)
#>     5. We show that we can be tough
#>     13. We not appear vulnerable
#>     21. We are aggressive if necessary
#>     29. We not show our weaknesses
#> DE: Be self-protective (180 degrees)
#>     2. We are the winners in any argument or dispute
#>     10. We do whatever is in our best interest
#>     18. We are better than them
#>     26. We keep our guard up
#> FG: Be wary (225 degrees)
#>     7. We let them fend for themselves
#>     15. They stay out of our business
#>     23. We not trust them
#>     31. We not get entangled in their affairs
#> HI: Be conflict-avoidant (270 degrees)
#>     4. We avoid conflict
#>     12. They not get angry with us
#>     20. We not get into arguments
#>     28. We not make them angry
#> JK: Be cooperative (315 degrees)
#>     1. We are friendly
#>     9. We celebrate their achievements
#>     17. They feel we are all on the same team
#>     25. We are cooperative
#> LM: Be understanding (360 degrees)
#>     6. We appreciate what they have to offer
#>     14. We understand their point of view
#>     22. We show concern for their welfare
#>     30. We are able to compromise
#> NO: Be respected (45 degrees)
#>     3. They respect what we have to say
#>     11. We get the chance to express our views
#>     19. They listen to what we have to say
#>     27. They see us as responsible
```
