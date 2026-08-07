# Inventory of Interpersonal Strengths

Information about the Inventory of Interpersonal Strengths (IIS-64).

## Usage

``` r
iis64
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Hatcher, R. L., & Rogers, D. T. (2009). Development and validation of a
measure of interpersonal strengths: The Inventory of Interpersonal
Strengths. *Psychological Assessment, 21*(4), 554-569.

[doi:10.1037/a0017269](https://doi.org/10.1037/a0017269)

## Examples

``` r
summary(iis64)
#> IIS-64: Inventory of Interpersonal Strengths
#> 64 items, 8 scales, 1 normative data sets
#> Hatcher & Rogers (2009)
#> <https://doi.org/10.1037/a0017269>
#> 
#> The IIS-64 contains 8 circumplex scales.
#> PA: Lead (90 degrees)
#> BC: Direct (135 degrees)
#> DE: Balance (180 degrees)
#> FG: Restrain (225 degrees)
#> HI: Cooperate (270 degrees)
#> JK: Consider (315 degrees)
#> LM: Connect (360 degrees)
#> NO: Engage (45 degrees)
#> 
#> The IIS-64 is rated using the following 6-point scale.
#> 1. Very little like me
#> 2. Something like me
#> 3. Moderately like me
#> 4. Quite a bit like me
#> 5. Very like me
#> 6. Almost always like me
#> 
#> The IIS-64 contains 64 items (partial text):
#> 1. I can really shine in the spotlight
#> 2. I enjoy learning from people who have more experience than I do
#> 3. It's important to me to be honest even when it's difficult
#> 4. I recover quickly when people hurt my feelings
#> 5. I realize that I don't have to be friends with everyone
#> 6. I enjoy being with other people
#> 7. I can assert my needs even when it's not agreeable to others
#> 8. When someone irritates me, I look for a constructive solution
#> 9. I'm pretty even-tempered with others
#> 10. I can make a decision even if others disagree
#> 11. I don't give up easily in competitive situations
#> 12. I feel comfortable being open about myself
#> 13. I feel good when I'm with other people
#> 14. I am able to compromise
#> 15. I can help others with their needs without neglecting my own
#> 16. I feel confident in front of other people
#> 17. I enjoy celebrating others' achievements
#> 18. I can say 'no' to others
#> 19. I'm warm with other people
#> 20. I argue effectively with others
#> 21. I can listen and think before I act in relationships
#> 22. I enjoy lively competition with others
#> 23. I look forward to spending time with people
#> 24. I put myself out there in order to connect with others
#> 25. I try to regain contact with people with whom I've lost touch
#> 26. Offering other people emotional support is important to me
#> 27. I stick by my friends when they're in trouble
#> 28. I know how to look after my own interests
#> 29. I can be very persuasive
#> 30. I make time to be with others
#> 31. I am a strong but fair leader
#> 32. I show my gratitude for what others do for me
#> 33. It makes me happy when others are happy
#> 34. I hesitate to express opinions about others without all the facts
#> 35. My feelings of gratitude warm my relationships with others
#> 36. I like to be clear on my agreements with other people
#> 37. I can ask other people for what I want
#> 38. I enjoy mingling at parties
#> 39. I can take care of myself, even when others' needs feel pressing
#> 40. I can take charge in a group
#> 41. I work really well as an assistant
#> 42. When friends ask for favors, I'm delighted to help them out
#> 43. I feel enriched by helping others
#> 44. I'm excited about meeting new people
#> 45. I recognize when others need privacy
#> 46. I can let other people know when I think that they're asking for too much from me
#> 47. I'm a good listener
#> 48. I can rely on myself when I'm having problems with others
#> 49. I'm cooperative
#> 50. I'm ok with not being included in all activities
#> 51. I can be interested in others without being nosy
#> 52. I enjoy complimenting others
#> 53. I know how to be angry without pushing people away
#> 54. I can resist others' tempting me to indulge myself
#> 55. I'm comfortable disagreeing with others
#> 56. I'm respectful of others' need for time to themselves
#> 57. I approach other people with friendliness
#> 58. I am able to be assertive with other people
#> 59. When others get me down, I can bounce back
#> 60. I don't ask others for more than they are comfortable giving
#> 61. I like asking people about their lives
#> 62. I try to help people to loosen up
#> 63. I can make people laugh
#> 64. I put other people at ease
#> 
#> The IIS-64 currently has 1 normative data set(s):
#> 1. 684 American college students
#> Hatcher & Rogers (2009)
#> <https://doi.org/10.1037/a0017269>
scales(iis64, items = TRUE)
#> The IIS-64 contains 8 circumplex scales.
#> PA: Lead (90 degrees)
#>     11. I don't give up easily in competitive situations
#>     16. I feel confident in front of other people
#>     22. I enjoy lively competition with others
#>     29. I can be very persuasive
#>     31. I am a strong but fair leader
#>     37. I can ask other people for what I want
#>     40. I can take charge in a group
#>     58. I am able to be assertive with other people
#> BC: Direct (135 degrees)
#>     7. I can assert my needs even when it's not agreeable to others
#>     10. I can make a decision even if others disagree
#>     20. I argue effectively with others
#>     28. I know how to look after my own interests
#>     39. I can take care of myself, even when others' needs feel pressing
#>     46. I can let other people know when I think that they're asking for too much from me
#>     55. I'm comfortable disagreeing with others
#>     59. When others get me down, I can bounce back
#> DE: Balance (180 degrees)
#>     4. I recover quickly when people hurt my feelings
#>     5. I realize that I don't have to be friends with everyone
#>     15. I can help others with their needs without neglecting my own
#>     18. I can say 'no' to others
#>     36. I like to be clear on my agreements with other people
#>     48. I can rely on myself when I'm having problems with others
#>     50. I'm ok with not being included in all activities
#>     53. I know how to be angry without pushing people away
#> FG: Restrain (225 degrees)
#>     3. It's important to me to be honest even when it's difficult
#>     8. When someone irritates me, I look for a constructive solution
#>     21. I can listen and think before I act in relationships
#>     34. I hesitate to express opinions about others without all the facts
#>     45. I recognize when others need privacy
#>     51. I can be interested in others without being nosy
#>     54. I can resist others' tempting me to indulge myself
#>     56. I'm respectful of others' need for time to themselves
#> HI: Cooperate (270 degrees)
#>     2. I enjoy learning from people who have more experience than I do
#>     9. I'm pretty even-tempered with others
#>     14. I am able to compromise
#>     32. I show my gratitude for what others do for me
#>     41. I work really well as an assistant
#>     47. I'm a good listener
#>     49. I'm cooperative
#>     60. I don't ask others for more than they are comfortable giving
#> JK: Consider (315 degrees)
#>     17. I enjoy celebrating others' achievements
#>     26. Offering other people emotional support is important to me
#>     27. I stick by my friends when they're in trouble
#>     33. It makes me happy when others are happy
#>     35. My feelings of gratitude warm my relationships with others
#>     42. When friends ask for favors, I'm delighted to help them out
#>     43. I feel enriched by helping others
#>     52. I enjoy complimenting others
#> LM: Connect (360 degrees)
#>     6. I enjoy being with other people
#>     13. I feel good when I'm with other people
#>     19. I'm warm with other people
#>     23. I look forward to spending time with people
#>     30. I make time to be with others
#>     44. I'm excited about meeting new people
#>     57. I approach other people with friendliness
#>     61. I like asking people about their lives
#> NO: Engage (45 degrees)
#>     1. I can really shine in the spotlight
#>     12. I feel comfortable being open about myself
#>     24. I put myself out there in order to connect with others
#>     25. I try to regain contact with people with whom I've lost touch
#>     38. I enjoy mingling at parties
#>     62. I try to help people to loosen up
#>     63. I can make people laugh
#>     64. I put other people at ease
```
