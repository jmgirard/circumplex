# Circumplex Scales of Interpersonal Values

Information about the Circumplex Scales of Interpersonal Values (CSIV).

## Usage

``` r
csiv
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Locke, K. D. (2000). Circumplex scales of interpersonal values:
Reliability, validity, and applicability to interpersonal problems and
personality disorders. *Journal of Personality Assessment, 75*(2),
249-267. This article defines the instrument; it reports no octant means
or standard deviations, and its samples are not the normative sample
below.

Norms: Locke, K. D. (n.d.). *CSIV norms* \[unpublished data\]. The
shipped octant means and standard deviations, and the N of 1,200, come
from the author's norms table:
<https://kennethlocke.org/CSIV/CSIV_Norms.html>

<https://kennethlocke.org/CSIV/CSIV.html>

## Examples

``` r
summary(csiv)
#> CSIV: Circumplex Scales of Interpersonal Values
#> 64 items, 8 scales, 1 normative data sets
#> Locke (2000)
#> <https://doi.org/10.1207/S15327752JPA7502_6>
#> 
#> The CSIV contains 8 circumplex scales.
#> PA: +A (90 degrees)
#> BC: +A-C (135 degrees)
#> DE: -C (180 degrees)
#> FG: -A-C (225 degrees)
#> HI: -A (270 degrees)
#> JK: -A+C (315 degrees)
#> LM: +C (360 degrees)
#> NO: +A+C (45 degrees)
#> 
#> The CSIV is rated using the following 5-point scale.
#> 0. Not important to me
#> 1. Mildly important to me
#> 2. Moderately important to me
#> 3. Very important to me
#> 4. Extremely important to me
#> 
#> The CSIV contains 64 items (open-access):
#> Prefix: When I am with him/her/them, it is...
#> 1. That I appear confident
#> 2. That I not reveal my positive feelings for them
#> 3. That I feel connected to them
#> 4. That I appear forceful
#> 5. That I conform to their expectations
#> 6. That I am unique
#> 7. That I keep my guard up
#> 8. That I put their needs before mine
#> 9. That they acknowledge when I am right
#> 10. That I not make a social blunder
#> 11. That they show interest in what I have to say
#> 12. That I attack back when I am attacked
#> 13. That I not get into an argument
#> 14. That they not deceive me
#> 15. That they not know what I am thinking or feelings
#> 16. That they not see me as getting in their way
#> 17. That I get the chance to voice my views
#> 18. That I appear aloof
#> 19. That they support me when I am having problems
#> 20. That I keep the upper hand
#> 21. That I do what they want me to do
#> 22. That I express myself openly
#> 23. That I not show I care about them
#> 24. That I get along with them
#> 25. That they respect my privacy
#> 26. That I not make mistakes in front of them
#> 27. That they understand me
#> 28. That I put my needs first
#> 29. That I live up to their expectations
#> 30. That they respect what I have to say
#> 31. That they keep their distance from me
#> 32. That they not reject me
#> 33. That I not back down when disagreements arise
#> 34. That I not say something stupid
#> 35. That they come to me with their problems
#> 36. That I am the one in charge
#> 37. That I not make them angry
#> 38. That I have an impact on them
#> 39. That I do better than them
#> 40. That I make them feel happy
#> 41. That they not tell me what to do
#> 42. That I not expose myself to the possibility of rejection
#> 43. That they are considerate
#> 44. That I avenge insults and injustices against me
#> 45. That I go along with what they want to do
#> 46. That they show me respect
#> 47. That they see me as cool and unemotional
#> 48. That they approve of me
#> 49. That I am obeyed when I am in authority
#> 50. That I not expose myself to ridicule
#> 51. That they stay with me when things aren't going well
#> 52. That I win if there is an argument
#> 53. That I not embarrass myself
#> 54. That they see me as responsible
#> 55. That I appear detached
#> 56. That they think I am a nice person
#> 57. That they admit it when they are wrong
#> 58. That I keep my thoughts or feelings to myself
#> 59. That they show concern for how I am feeling
#> 60. That they mind their own business
#> 61. That they not get angry with me
#> 62. That they listen to what I have to say
#> 63. That I not reveal what I am really like
#> 64. That they not get their feelings hurt
#> 
#> The CSIV currently has 1 normative data set(s):
#> 1. 1200 American college students
#> Locke (n.d.); instrument published as Locke (2000)
#> <https://kennethlocke.org/CSIV/CSIV_Norms.html>
scales(csiv, items = TRUE)
#> The CSIV contains 8 circumplex scales.
#> PA: +A (90 degrees)
#>     1. That I appear confident
#>     9. That they acknowledge when I am right
#>     17. That I get the chance to voice my views
#>     25. That they respect my privacy
#>     33. That I not back down when disagreements arise
#>     41. That they not tell me what to do
#>     49. That I am obeyed when I am in authority
#>     57. That they admit it when they are wrong
#> BC: +A-C (135 degrees)
#>     4. That I appear forceful
#>     12. That I attack back when I am attacked
#>     20. That I keep the upper hand
#>     28. That I put my needs first
#>     36. That I am the one in charge
#>     44. That I avenge insults and injustices against me
#>     52. That I win if there is an argument
#>     60. That they mind their own business
#> DE: -C (180 degrees)
#>     7. That I keep my guard up
#>     15. That they not know what I am thinking or feelings
#>     23. That I not show I care about them
#>     31. That they keep their distance from me
#>     39. That I do better than them
#>     47. That they see me as cool and unemotional
#>     55. That I appear detached
#>     63. That I not reveal what I am really like
#> FG: -A-C (225 degrees)
#>     2. That I not reveal my positive feelings for them
#>     10. That I not make a social blunder
#>     18. That I appear aloof
#>     26. That I not make mistakes in front of them
#>     34. That I not say something stupid
#>     42. That I not expose myself to the possibility of rejection
#>     50. That I not expose myself to ridicule
#>     58. That I keep my thoughts or feelings to myself
#> HI: -A (270 degrees)
#>     5. That I conform to their expectations
#>     13. That I not get into an argument
#>     21. That I do what they want me to do
#>     29. That I live up to their expectations
#>     37. That I not make them angry
#>     45. That I go along with what they want to do
#>     53. That I not embarrass myself
#>     61. That they not get angry with me
#> JK: -A+C (315 degrees)
#>     8. That I put their needs before mine
#>     16. That they not see me as getting in their way
#>     24. That I get along with them
#>     32. That they not reject me
#>     40. That I make them feel happy
#>     48. That they approve of me
#>     56. That they think I am a nice person
#>     64. That they not get their feelings hurt
#> LM: +C (360 degrees)
#>     3. That I feel connected to them
#>     11. That they show interest in what I have to say
#>     19. That they support me when I am having problems
#>     27. That they understand me
#>     35. That they come to me with their problems
#>     43. That they are considerate
#>     51. That they stay with me when things aren't going well
#>     59. That they show concern for how I am feeling
#> NO: +A+C (45 degrees)
#>     6. That I am unique
#>     14. That they not deceive me
#>     22. That I express myself openly
#>     30. That they respect what I have to say
#>     38. That I have an impact on them
#>     46. That they show me respect
#>     54. That they see me as responsible
#>     62. That they listen to what I have to say
```
