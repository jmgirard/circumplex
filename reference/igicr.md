# Interpersonal Goals Inventory for Children, Revised Version

Information about the Interpersonal Goals Inventory for Children,
Revised Version (IGI-CR).

## Usage

``` r
igicr
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Trucco, E. M., Wright, A. G. C., & Colder, C. R. (2013). A revised
interpersonal circumplex inventory of children’s social goals.
*Assessment, 20*(1), 98-113.

[doi:10.1177/1073191111411672](https://doi.org/10.1177/1073191111411672)

## Examples

``` r
summary(igicr)
#> IGI-CR: Interpersonal Goals Inventory for Children, Revised Version
#> 32 items, 8 scales, 3 normative data sets
#> Trucco, Wright, & Colder (2013)
#> <https://doi.org/10.1177/1073191111411672>
#> 
#> The IGI-CR contains 8 circumplex scales.
#> PA: +A (90 degrees)
#> BC: +A-C (135 degrees)
#> DE: -C (180 degrees)
#> FG: -A-C (225 degrees)
#> HI: -A (270 degrees)
#> JK: -A+C (315 degrees)
#> LM: +C (360 degrees)
#> NO: +A+C (45 degrees)
#> 
#> The IGI-CR is rated using the following 5-point scale.
#> 0. Not at all important to me
#> 1. Somewhat important to me
#> 2. Important to me
#> 3. Very important to me
#> 4. Extremely important to me
#> 
#> The IGI-CR contains 32 items (open-access):
#> Prefix: When with your peers, in general how important is it to you that...
#> 1. Your peers respect and admire you
#> 2. Your peers agree to do what you suggest
#> 3. You do not show your feelings in front of your peers
#> 4. You do not do anything ridiculous
#> 5. Your peers do not get angry with you
#> 6. Everyone feels good
#> 7. You feel close to your peers
#> 8. You say exactly what you want
#> 9. You appear self-confident and make an impression on your peers
#> 10. You get to decide what to play
#> 11. You do not give away too much about yourself
#> 12. You do not say stupid things when your peers are listening
#> 13. You do not make your peers angry
#> 14. You can put your peers in a good mood
#> 15. Real friendship develops between you
#> 16. Your peers listen to your opinion
#> 17. Your peers think you are smart
#> 18. The group does what you say
#> 19. You keep your thoughts to yourself
#> 20. Your peers do not laugh or make fun of you
#> 21. You do not annoy your peers
#> 22. You are able to please your peers
#> 23. Your peers help you when you have a problem
#> 24. You can state your opinion
#> 25. You don't back down when there is a disagreement
#> 26. You feel you have control over your peers
#> 27. You do not let your peers get too close to you
#> 28. You do not make a fool of yourself in front of your peers
#> 29. You let your peers make decisions
#> 30. You agree with your peers about things
#> 31. Your peers come to you when they have a problem
#> 32. You are able to tell your peers how you feel
#> 
#> The IGI-CR currently has 3 normative data set(s):
#> 1. 387 American community adolescents (age 11-13), overall
#> Reference kind: identified published source
#> Trucco, Wright, & Colder (2013)
#> <https://doi.org/10.1177/1073191111411672>
#> 2. 174 American community adolescents (age 11-13), males
#> Reference kind: identified published source
#> Trucco, Wright, & Colder (2013)
#> <https://doi.org/10.1177/1073191111411672>
#> 3. 213 American community adolescents (age 11-13), females
#> Reference kind: identified published source
#> Trucco, Wright, & Colder (2013)
#> <https://doi.org/10.1177/1073191111411672>
scales(igicr, items = TRUE)
#> The IGI-CR contains 8 circumplex scales.
#> PA: +A (90 degrees)
#>     1. Your peers respect and admire you
#>     9. You appear self-confident and make an impression on your peers
#>     17. Your peers think you are smart
#>     25. You don't back down when there is a disagreement
#> BC: +A-C (135 degrees)
#>     2. Your peers agree to do what you suggest
#>     10. You get to decide what to play
#>     18. The group does what you say
#>     26. You feel you have control over your peers
#> DE: -C (180 degrees)
#>     3. You do not show your feelings in front of your peers
#>     11. You do not give away too much about yourself
#>     19. You keep your thoughts to yourself
#>     27. You do not let your peers get too close to you
#> FG: -A-C (225 degrees)
#>     4. You do not do anything ridiculous
#>     12. You do not say stupid things when your peers are listening
#>     20. Your peers do not laugh or make fun of you
#>     28. You do not make a fool of yourself in front of your peers
#> HI: -A (270 degrees)
#>     5. Your peers do not get angry with you
#>     13. You do not make your peers angry
#>     21. You do not annoy your peers
#>     29. You let your peers make decisions
#> JK: -A+C (315 degrees)
#>     6. Everyone feels good
#>     14. You can put your peers in a good mood
#>     22. You are able to please your peers
#>     30. You agree with your peers about things
#> LM: +C (360 degrees)
#>     7. You feel close to your peers
#>     15. Real friendship develops between you
#>     23. Your peers help you when you have a problem
#>     31. Your peers come to you when they have a problem
#> NO: +A+C (45 degrees)
#>     8. You say exactly what you want
#>     16. Your peers listen to your opinion
#>     24. You can state your opinion
#>     32. You are able to tell your peers how you feel
```
