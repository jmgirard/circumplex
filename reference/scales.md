# Display the scales of a circumplex instrument

Display the scales of a circumplex instrument including the total number
of scales and each scale's abbreviation, hypothetical angle, and text
label.

## Usage

``` r
scales(x, items = FALSE)
```

## Arguments

- x:

  Required. An object of the instrument class.

- items:

  Optional. A logical determining whether the items for each scale
  should be displayed below its other information (default = FALSE).

## Value

The same input object. Prints text to console.

## See also

Other instrument functions:
[`anchors()`](http://circumplex.jmgirard.com/reference/anchors.md),
[`instruments()`](http://circumplex.jmgirard.com/reference/instruments.md),
[`items()`](http://circumplex.jmgirard.com/reference/items.md),
[`norms()`](http://circumplex.jmgirard.com/reference/norms.md)

## Examples

``` r
scales(csip)
#> The CSIP contains 8 circumplex scales.
#> PA: Domineering (90 degrees)
#> BC: Self-Centered (135 degrees)
#> DE: Distant (180 degrees)
#> FG: Socially Inhibited (225 degrees)
#> HI: Nonassertive (270 degrees)
#> JK: Exploitable (315 degrees)
#> LM: Self-Sacrificing (360 degrees)
#> NO: Intrusive (45 degrees)
scales(csip, items = TRUE)
#> The CSIP contains 8 circumplex scales.
#> PA: Domineering (90 degrees)
#>     1. Bossing around other people too much
#>     9. Verbally or physically abusing others
#>     17. Starting arguments and conflicts with others
#>     25. Trying to influence or control other people too much
#>     33. Dominating or intimidating others
#>     41. Acting aggressively toward others
#>     49. Manipulating other people to get what I want
#>     57. Acting superior or condescending toward others
#> BC: Self-Centered (135 degrees)
#>     2. Acting rude and inconsiderate toward others
#>     10. Acting selfishly with others
#>     18. Being unable to feel guilt or remorse
#>     26. Lacking respect for other people's beliefs, attitudes, or opinions
#>     34. Having trouble getting along with others
#>     42. Being insensitive to the thoughts, feelings, and needs of others
#>     50. Disliking most people
#>     58. Having trouble giving emotional or moral support to others
#> DE: Distant (180 degrees)
#>     3. Pushing away from other people who get too close
#>     11. Difficulty showing love and affection to others
#>     19. Being unable to enjoy the company of others
#>     27. Feeling emotionally disconnected from others
#>     35. Difficulty developing close and lasting relationships
#>     43. Being unable to fully connect with others
#>     51. Difficulty opening up to others
#>     59. Feeling uncomfortable with being close or intimate with others
#> FG: Socially Inhibited (225 degrees)
#>     4. Difficulty making friends
#>     12. Having trouble fitting in with others
#>     20. Avoiding people or social situations
#>     28. Being unable to keep conversations going
#>     36. Feeling like an outsider in most social situations
#>     44. Being unable to be myself around others
#>     52. Feeling fearful or nervous in social situations
#>     60. Acting shy around others
#> HI: Nonassertive (270 degrees)
#>     5. Lacking self-confidence
#>     13. Getting easily embarrassed in front of others
#>     21. Difficulty taking the lead
#>     29. Having trouble asserting myself
#>     37. Feeling weak and insecure around dominant others
#>     45. Being unable to stand up to others
#>     53. Avoiding confrontation when problems arise
#>     61. Letting other people make decisions too often
#> JK: Exploitable (315 degrees)
#>     6. Letting other people boss me around too much
#>     14. Acting overly submissive with others
#>     22. Being unable to express anger toward others
#>     30. Being too concerned about what other people think
#>     38. Being easily taken advantage of
#>     46. Compromising with other people too much
#>     54. Being easily influenced by others
#>     62. Being unable to say 'no'
#> LM: Self-Sacrificing (360 degrees)
#>     7. Putting other people's needs before my own too much
#>     15. Giving too much to others
#>     23. Forgiving people too easily
#>     31. Being overly sentimental or tender-hearted
#>     39. Being easily affected by the pain and suffering of others
#>     47. Trusting people too easily
#>     55. Trying to solve other people's problems too much
#>     63. Getting too attached to others
#> NO: Intrusive (45 degrees)
#>     8. Being overly affectionate with others
#>     16. Difficulty keeping personal matters private from others
#>     24. Talking too much
#>     32. Flirting with other people too much
#>     40. Having trouble respecting other people's privacy
#>     48. Exaggerating so that other people will respect me
#>     56. Confronting people too quickly about problems
#>     64. Needing to be the center of attention
```
