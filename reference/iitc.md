# Inventory of Influence Tactics Circumplex

Information about the Inventory of Influence Tactics Circumplex (IIT-C).

## Usage

``` r
iitc
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Bliton, C. F., & Pincus, A. L. (in press). Construction and Validation
of the Interpersonal Influence Tactics Circumplex (IIT-C) Scales.
*Assessment*.

[doi:10.1177/1073191119864661](https://doi.org/10.1177/1073191119864661)

## Examples

``` r
summary(iitc)
#> IIT-C: Inventory of Influence Tactics Circumplex
#> 64 items, 8 scales, 1 normative data sets
#> Bliton & Pincus (2019)
#> <https://doi.org/10.1177/1073191119864661>
#> 
#> The IIT-C contains 8 circumplex scales.
#> PA: Dominant (90 degrees)
#> BC: Calculating (135 degrees)
#> DE: Cold (180 degrees)
#> FG: Self-Critical (225 degrees)
#> HI: Submissive (270 degrees)
#> JK: Ingratiating (315 degrees)
#> LM: Warm (360 degrees)
#> NO: Gregarious (45 degrees)
#> 
#> The IIT-C is rated using the following 6-point scale.
#> 0. Not at all likely
#> 1. Slightly likely
#> 2. Somewhat likely
#> 3. Quite likely
#> 4. Very much likely
#> 5. Extremely likely
#> 
#> The IIT-C contains 64 items (open-access):
#> Prefix: When interacting with others, how likely are you to influence them by...
#> 1. Daring them to do it
#> 2. Reminding them that I am in charge so they do it
#> 3. Excluding them so they do it
#> 4. Getting someone else to ask them to do it
#> 5. Making them feel sorry for me so they do it
#> 6. Begging them to do it
#> 7. Suggesting we should do it together
#> 8. Praising them so they do it
#> 9. Impressing them to get my way
#> 10. Reminding them that they owe me so they do it
#> 11. Blaming them so they do it
#> 12. Showing them that I am clueless so they do it
#> 13. Doing nothing until they do it
#> 14. Crying until they do it
#> 15. Telling them I'd do anything in return so they do it
#> 16. Giving them advice so they do it
#> 17. Using humor to get what I want
#> 18. Tricking them into doing what I want
#> 19. Getting angry at them so they do it
#> 20. Telling them I don't know how to do it so they do it
#> 21. Waiting for them to do it
#> 22. Promising to do something in return so they do it
#> 23. Being patient with them so they do it
#> 24. Being a role model so they do it
#> 25. Taking charge of the conversation to get my way
#> 26. Misleading them into doing what I want
#> 27. Guilt tripping them until they do it
#> 28. Making self-critical comments to get what I want
#> 29. Telling them that they are better at it than I am so they do it
#> 30. Asking for their help so they do it
#> 31. Telling them how much I appreciate them doing it
#> 32. Telling them how exciting it is so they do it
#> 33. Assigning them the responsibility to do it
#> 34. Making them doubt themselves to get what I want
#> 35. Being passive aggressive until they do it
#> 36. Acting dependent on them so they do it
#> 37. Pouting to get what I want
#> 38. Offering a compromise so they do it
#> 39. Being kind to them so they do it
#> 40. Being affectionate with them so they do it
#> 41. Forcing them to do it
#> 42. Criticizing them so they do it
#> 43. Ignoring them until they do it
#> 44. Whining about it so they do it
#> 45. Sulking so they do it
#> 46. Telling them how much it means to me so they do it
#> 47. Showing them how to do it
#> 48. Becoming enthusiastic about it so they do it
#> 49. Using my authority to get my way
#> 50. Lying to get what I want
#> 51. Exaggerating my problems so they do it
#> 52. Making them pity me to get what I want
#> 53. Clinging to them until they do it
#> 54. Asking politely so they do it
#> 55. Encouraging them to do it
#> 56. Flattering them so they do it
#> 57. Controlling them so they do it
#> 58. Arguing about it until they do it
#> 59. Holding a grudge against them until they do it
#> 60. Putting myself down to get what I want
#> 61. Hinting at what I want them to do
#> 62. Telling them how grateful I will be if they do it
#> 63. Being a good example for them so they do it
#> 64. Using charm to get my way
#> 
#> The IIT-C currently has 1 normative data set(s):
#> 1. 862 American college students
#> Bliton & Pincus (2019)
#> <https://doi.org/10.1177/1073191119864661>
scales(iitc, items = TRUE)
#> The IIT-C contains 8 circumplex scales.
#> PA: Dominant (90 degrees)
#>     1. Daring them to do it
#>     9. Impressing them to get my way
#>     17. Using humor to get what I want
#>     25. Taking charge of the conversation to get my way
#>     33. Assigning them the responsibility to do it
#>     41. Forcing them to do it
#>     49. Using my authority to get my way
#>     57. Controlling them so they do it
#> BC: Calculating (135 degrees)
#>     2. Reminding them that I am in charge so they do it
#>     10. Reminding them that they owe me so they do it
#>     18. Tricking them into doing what I want
#>     26. Misleading them into doing what I want
#>     34. Making them doubt themselves to get what I want
#>     42. Criticizing them so they do it
#>     50. Lying to get what I want
#>     58. Arguing about it until they do it
#> DE: Cold (180 degrees)
#>     3. Excluding them so they do it
#>     11. Blaming them so they do it
#>     19. Getting angry at them so they do it
#>     27. Guilt tripping them until they do it
#>     35. Being passive aggressive until they do it
#>     43. Ignoring them until they do it
#>     51. Exaggerating my problems so they do it
#>     59. Holding a grudge against them until they do it
#> FG: Self-Critical (225 degrees)
#>     4. Getting someone else to ask them to do it
#>     12. Showing them that I am clueless so they do it
#>     20. Telling them I don't know how to do it so they do it
#>     28. Making self-critical comments to get what I want
#>     36. Acting dependent on them so they do it
#>     44. Whining about it so they do it
#>     52. Making them pity me to get what I want
#>     60. Putting myself down to get what I want
#> HI: Submissive (270 degrees)
#>     5. Making them feel sorry for me so they do it
#>     13. Doing nothing until they do it
#>     21. Waiting for them to do it
#>     29. Telling them that they are better at it than I am so they do it
#>     37. Pouting to get what I want
#>     45. Sulking so they do it
#>     53. Clinging to them until they do it
#>     61. Hinting at what I want them to do
#> JK: Ingratiating (315 degrees)
#>     6. Begging them to do it
#>     14. Crying until they do it
#>     22. Promising to do something in return so they do it
#>     30. Asking for their help so they do it
#>     38. Offering a compromise so they do it
#>     46. Telling them how much it means to me so they do it
#>     54. Asking politely so they do it
#>     62. Telling them how grateful I will be if they do it
#> LM: Warm (360 degrees)
#>     7. Suggesting we should do it together
#>     15. Telling them I'd do anything in return so they do it
#>     23. Being patient with them so they do it
#>     31. Telling them how much I appreciate them doing it
#>     39. Being kind to them so they do it
#>     47. Showing them how to do it
#>     55. Encouraging them to do it
#>     63. Being a good example for them so they do it
#> NO: Gregarious (45 degrees)
#>     8. Praising them so they do it
#>     16. Giving them advice so they do it
#>     24. Being a role model so they do it
#>     32. Telling them how exciting it is so they do it
#>     40. Being affectionate with them so they do it
#>     48. Becoming enthusiastic about it so they do it
#>     56. Flattering them so they do it
#>     64. Using charm to get my way
```
