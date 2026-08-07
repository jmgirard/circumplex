# Inventory of Interpersonal Strengths, Brief Version

Information about the Inventory of Interpersonal Strengths, Brief
Version (IIS-32).

## Usage

``` r
iis32
```

## Format

An object of class `circumplex_instrument` of length 5.

## Source

Hatcher, R. L., & Rogers, D. T. (2012). The IIS-32: A brief inventory of
interpersonal strengths. *Journal of Personality Assessment, 94*(6),
638-646. This article defines the instrument and supplies the
item-to-octant grouping (Appendix, p. 646); it reports no octant means
or standard deviations, and none of its samples is the normative sample
below.

Norms: source unconfirmed. The shipped octant means and standard
deviations, the N of 1380, and the sample description that accompanies
them are published in no source that has been identified, including the
article above. Treat them as unverified until a source is established.

[doi:10.1080/00223891.2012.681818](https://doi.org/10.1080/00223891.2012.681818)

## Examples

``` r
summary(iis32)
#> IIS-32: Inventory of Interpersonal Strengths, Brief Version
#> 32 items, 8 scales, 1 normative data sets
#> Hatcher & Rogers (2012)
#> <https://doi.org/10.1080/00223891.2012.681818>
#> 
#> The IIS-32 contains 8 circumplex scales.
#> PA: Lead (90 degrees)
#> BC: Direct (135 degrees)
#> DE: Balance (180 degrees)
#> FG: Restrain (225 degrees)
#> HI: Cooperate (270 degrees)
#> JK: Consider (315 degrees)
#> LM: Connect (360 degrees)
#> NO: Engage (45 degrees)
#> 
#> The IIS-32 is rated using the following 6-point scale.
#> 1. Very little like me
#> 2. Something like me
#> 3. Moderately like me
#> 4. Quite a bit like me
#> 5. Very like me
#> 6. Almost always like me
#> 
#> The IIS-32 contains 32 items (partial text):
#> 1. I can really shine in the spotlight
#> 2. I enjoy being with other people
#> 3. I can make a decision even if others disagree
#> 4. I feel comfortable being open about myself
#> 5. I am able to compromise
#> 6. I can help others with their needs without neglecting my own
#> 7. I enjoy celebrating others' achievements
#> 8. I can say 'no' to others
#> 9. I'm warm with other people
#> 10. I argue effectively with others
#> 11. I can listen and think before I act in relationships
#> 12. I put myself out there in order to connect with others
#> 13. Offering other people emotional support is important to me
#> 14. I can be very persuasive
#> 15. I make time to be with others
#> 16. I am a strong but fair leader
#> 17. I show my gratitude for what others do for me
#> 18. I can ask other people for what I want
#> 19. I can take care of myself, even when others' needs feel pressing
#> 20. I can take charge in a group
#> 21. When friends ask for favors, I'm delighted to help them out
#> 22. I feel enriched by helping others
#> 23. I'm excited about meeting new people
#> 24. I recognize when others need privacy
#> 25. I can let other people know when I think that they're asking for too much from me
#> 26. I can rely on myself when I'm having problems with others
#> 27. I'm cooperative
#> 28. I'm okay with not being included in all activities
#> 29. I can be interested in others without being nosy
#> 30. I can resist others' tempting me to indulge myself
#> 31. I don't ask others for more than they are comfortable giving
#> 32. I put other people at ease
#> 
#> The IIS-32 currently has 1 normative data set(s):
#> 1. 1380 American college students
#> Norms source unconfirmed; instrument published as Hatcher & Rogers (2012)
#> <https://doi.org/10.1080/00223891.2012.681818>
scales(iis32, items = TRUE)
#> The IIS-32 contains 8 circumplex scales.
#> PA: Lead (90 degrees)
#>     14. I can be very persuasive
#>     16. I am a strong but fair leader
#>     18. I can ask other people for what I want
#>     20. I can take charge in a group
#> BC: Direct (135 degrees)
#>     3. I can make a decision even if others disagree
#>     10. I argue effectively with others
#>     19. I can take care of myself, even when others' needs feel pressing
#>     25. I can let other people know when I think that they're asking for too much from me
#> DE: Balance (180 degrees)
#>     6. I can help others with their needs without neglecting my own
#>     8. I can say 'no' to others
#>     26. I can rely on myself when I'm having problems with others
#>     28. I'm okay with not being included in all activities
#> FG: Restrain (225 degrees)
#>     11. I can listen and think before I act in relationships
#>     24. I recognize when others need privacy
#>     29. I can be interested in others without being nosy
#>     30. I can resist others' tempting me to indulge myself
#> HI: Cooperate (270 degrees)
#>     5. I am able to compromise
#>     17. I show my gratitude for what others do for me
#>     27. I'm cooperative
#>     31. I don't ask others for more than they are comfortable giving
#> JK: Consider (315 degrees)
#>     7. I enjoy celebrating others' achievements
#>     13. Offering other people emotional support is important to me
#>     21. When friends ask for favors, I'm delighted to help them out
#>     22. I feel enriched by helping others
#> LM: Connect (360 degrees)
#>     2. I enjoy being with other people
#>     9. I'm warm with other people
#>     15. I make time to be with others
#>     23. I'm excited about meeting new people
#> NO: Engage (45 degrees)
#>     1. I can really shine in the spotlight
#>     4. I feel comfortable being open about myself
#>     12. I put myself out there in order to connect with others
#>     32. I put other people at ease
```
