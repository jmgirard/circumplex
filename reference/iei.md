# Interpersonal Emotion Inventory

Information about the Interpersonal Emotion Inventory (IEI).

## Usage

``` r
iei
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Horner, M. S., Locke, K. D., & Hulsey, T. L. (in press). Assessing
affective dimensions of the interpersonal circumplex: development and
validation of the interpersonal emotion inventory. *Journal of
Personality Assessment*. Advanced online publication.

[doi:10.1080/00223891.2024.2400266](https://doi.org/10.1080/00223891.2024.2400266)

<https://kennethlocke.org/IEI/IEI.html>

## Examples

``` r
summary(iei)
#> IEI: Interpersonal Emotion Inventory
#> 64 items, 8 scales, 2 normative data sets
#> Horner, Locke, & Hulsey (2024)
#> <https://doi.org/10.1080/00223891.2024.2400266>
#> 
#> The IEI contains 8 circumplex scales.
#> PA: Confident-Impressive (90 degrees)
#> BC: Superior-Callous (135 degrees)
#> DE: Rejecting-Suspicious (180 degrees)
#> FG: Rejected-Ashamed (225 degrees)
#> HI: Insecure-Anxious (270 degrees)
#> JK: Needy-Empathic (315 degrees)
#> LM: Welcoming-Trusting (360 degrees)
#> NO: Included-Proud (45 degrees)
#> 
#> The IEI is rated using the following 5-point scale.
#> 0. Never feel that way
#> 1. Seldom feel that way
#> 2. Sometimes feel that way
#> 3. Often feel that way
#> 4. Almost always feel that way
#> 
#> The IEI contains 64 items (open-access):
#> Prefix: When I interact with or think about myself in relation to others, I feel...
#> 1. Admirable
#> 2. Confident in my strengths
#> 3. Sure of myself
#> 4. Self-confident
#> 5. Attractive
#> 6. Confident that I am impressive
#> 7. Like a winner
#> 8. Unapologetic about winning
#> 9. Unintimidated
#> 10. Fully in command
#> 11. Invincible
#> 12. Superior
#> 13. Unsympathetic to suckers
#> 14. Unconcerned about others' feelings
#> 15. Impatient with others' shortcomings
#> 16. Unforgiving
#> 17. Like I just don't care about others
#> 18. Hostile
#> 19. Disapproving of others
#> 20. Rejecting of others
#> 21. Like I want to abandon others
#> 22. Like I want no part of any group
#> 23. Resentment
#> 24. Doubtful that I can rely on others
#> 25. Alienated
#> 26. Under attack
#> 27. Distant from them
#> 28. Rejected
#> 29. Unwanted
#> 30. Ashamed of myself
#> 31. Worthless
#> 32. Like a loser
#> 33. Like I am a disappointment
#> 34. Unsure of myself
#> 35. Self-doubt
#> 36. Insecure
#> 37. Worried that I will be annoying to others
#> 38. Worried I will disappoint others
#> 39. Like I need to appease others
#> 40. Careful not to disappoint others
#> 41. Self-conscious
#> 42. That others know better
#> 43. Anxious to please others
#> 44. Like I want to console and comfort others
#> 45. Empathic
#> 46. Like I want to help others
#> 47. Accepting of others
#> 48. Compassionate and caring toward others
#> 49. Concerned about others' well-being
#> 50. Admiration for others
#> 51. Like I really care about others
#> 52. Gracious toward others
#> 53. Grateful for others' love and support
#> 54. Emotionally connected and attuned to others
#> 55. Trusting in others' kindness
#> 56. Supported by them
#> 57. Loving kindness
#> 58. Close to them
#> 59. Loved
#> 60. Welcomed and cared about
#> 61. Important to others
#> 62. Valued
#> 63. Worthy
#> 64. Proud of myself
#> 
#> The IEI currently has 2 normative data set(s):
#> 1. 1223 American undergraduate students
#> Horner, Locke, & Hulsey (2024)
#> <https://osf.io/w37dj/>
#> 2. 278 American crowdworkers
#> Horner, Locke, & Hulsey (2024)
#> <https://osf.io/w37dj/>
scales(iei, items = TRUE)
#> The IEI contains 8 circumplex scales.
#> PA: Confident-Impressive (90 degrees)
#>     1. Admirable
#>     2. Confident in my strengths
#>     3. Sure of myself
#>     4. Self-confident
#>     5. Attractive
#>     6. Confident that I am impressive
#>     7. Like a winner
#>     8. Unapologetic about winning
#> BC: Superior-Callous (135 degrees)
#>     9. Unintimidated
#>     10. Fully in command
#>     11. Invincible
#>     12. Superior
#>     13. Unsympathetic to suckers
#>     14. Unconcerned about others' feelings
#>     15. Impatient with others' shortcomings
#>     16. Unforgiving
#> DE: Rejecting-Suspicious (180 degrees)
#>     17. Like I just don't care about others
#>     18. Hostile
#>     19. Disapproving of others
#>     20. Rejecting of others
#>     21. Like I want to abandon others
#>     22. Like I want no part of any group
#>     23. Resentment
#>     24. Doubtful that I can rely on others
#> FG: Rejected-Ashamed (225 degrees)
#>     25. Alienated
#>     26. Under attack
#>     27. Distant from them
#>     28. Rejected
#>     29. Unwanted
#>     30. Ashamed of myself
#>     31. Worthless
#>     32. Like a loser
#> HI: Insecure-Anxious (270 degrees)
#>     33. Like I am a disappointment
#>     34. Unsure of myself
#>     35. Self-doubt
#>     36. Insecure
#>     37. Worried that I will be annoying to others
#>     38. Worried I will disappoint others
#>     39. Like I need to appease others
#>     40. Careful not to disappoint others
#> JK: Needy-Empathic (315 degrees)
#>     41. Self-conscious
#>     42. That others know better
#>     43. Anxious to please others
#>     44. Like I want to console and comfort others
#>     45. Empathic
#>     46. Like I want to help others
#>     47. Accepting of others
#>     48. Compassionate and caring toward others
#> LM: Welcoming-Trusting (360 degrees)
#>     49. Concerned about others' well-being
#>     50. Admiration for others
#>     51. Like I really care about others
#>     52. Gracious toward others
#>     53. Grateful for others' love and support
#>     54. Emotionally connected and attuned to others
#>     55. Trusting in others' kindness
#>     56. Supported by them
#> NO: Included-Proud (45 degrees)
#>     57. Loving kindness
#>     58. Close to them
#>     59. Loved
#>     60. Welcomed and cared about
#>     61. Important to others
#>     62. Valued
#>     63. Worthy
#>     64. Proud of myself
```
