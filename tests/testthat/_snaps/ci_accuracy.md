# print and summary snapshots (seeded)

    Code
      print(res)
    Output
      
      SSM CI accuracy, simulated at your n and settings (30 replications per condition; bootstrap intervals with 60 replicates at level 0.95)
      
        # Profile [All] (n = 120; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 100.0% -- borderline
          Amplitude      coverage 96.7% -- borderline
          Displacement   coverage 93.3% when certified -- borderline
          Guardrail      under a truly zero amplitude, displacement would be
                         certified 0.0% of the time (user-expectation benchmark
                         2.5%)
        Verdict: BORDERLINE -- elevation, amplitude, and certified displacement
        coverage rates are borderline at this number of replications; a larger
        `reps` would sharpen the verdict.

---

    Code
      summary(res)
    Output
      
      Mean scores; bootstrap, 60 replicates, level 0.95; 30 reps per condition.
      Population: Browne circular model (CPM); groups All = 120; elapsed <masked>.
      Ladder c = 1, 0.25, 0; certified if a_lci / (a_uci - a_lci) >= 0.35.
      
      Structure note: population simulated from a Browne circular model fit (m = 3,
      RMSEA = 0.038, SRMR = 0.046).
        The structure fits adequately (RMSEA <= 0.08, Browne & Cudeck, 1993; SRMR
        <= 0.08, Hu & Bentler, 1999), so the simulated population is a reasonable
        stand-in for yours.
        Boundary markers: Heywood communality; small correlation-function weight;
        ill-conditioned Hessian.
      
      Verdicts at c = 1 (as estimated), Bradley (1978) band via 95% Wilson CIs:
      
        # Profile [All] (n = 120; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 100.0% -- borderline
          Amplitude      coverage 96.7% -- borderline
          Displacement   coverage 93.3% when certified -- borderline
          Guardrail      under a truly zero amplitude, displacement would be
                         certified 0.0% of the time (user-expectation benchmark
                         2.5%)
        Verdict: BORDERLINE -- elevation, amplitude, and certified displacement
        coverage rates are borderline at this number of replications; a larger
        `reps` would sharpen the verdict.
      
      Coverage by condition (d_cert: d when certified; cert: certification rate):
       Profile Condition   e     x     y     a     d d_cert  cert Structural
           All      1.00 1.0 0.967 0.900 0.967 0.933  0.933 1.000      FALSE
           All      0.25 0.9 0.967 0.867 0.933 0.933  0.818 0.367      FALSE
           All      0.00 0.9 0.967 1.000 0.000    NA     NA 0.000       TRUE
        Note: amplitude coverage on rows flagged Structural is structurally 0 (a
        percentile interval of strictly positive amplitude replicates cannot
        contain a zero truth) -- a theorem, not a measurement; the informative
        near-zero rungs are the small c > 0 ones.

# contrast print block reports displacement unconditionally (M15 snapshot)

    Code
      print(res)
    Output
      
      SSM CI accuracy, simulated at your n and settings (12 replications per condition; bootstrap intervals with 60 replicates at level 0.95)
      
        # Profile [Female] (n = 118; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 91.7% -- borderline
          Amplitude      coverage 75.0% -- INADEQUATE (under-coverage; misses fall
                         on both sides of the interval)
          Displacement   coverage 83.3% when certified -- borderline
          Guardrail      if the true amplitude were zero, displacement would still
                         be certified 16.7% of the time -- far more often than the
                         2.5% error rate the guardrail's wording suggests
        Verdict: CAUTION -- amplitude CIs are less reliable than nominal at this
        sample size and the interpretability guardrail certifies a truly zero
        amplitude more often than its wording suggests. Elevation and certified
        displacement coverage rates are borderline at this number of replications;
        a larger `reps` would sharpen the verdict. Consider a larger sample or
        treat near-zero amplitudes as inconclusive rather than absent.
      
        # Profile [Male] (n = 122; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 91.7% -- borderline
          Amplitude      coverage 75.0% -- INADEQUATE (under-coverage; misses are
                         almost all below the interval: the amplitude CI tends to
                         sit above the truth)
          Displacement   coverage 66.7% when certified -- INADEQUATE
                         (under-coverage)
          Guardrail      if the true amplitude were zero, displacement would still
                         be certified 16.7% of the time -- far more often than the
                         2.5% error rate the guardrail's wording suggests
        Verdict: CAUTION -- amplitude CIs are less reliable than nominal at this
        sample size, displacement CIs mis-cover even when certified, and the
        interpretability guardrail certifies a truly zero amplitude more often than
        its wording suggests. Elevation coverage is borderline at this number of
        replications; a larger `reps` would sharpen the verdict. Consider a larger
        sample or treat near-zero amplitudes as inconclusive rather than absent.
      
        # Contrast [Male - Female] (95% bootstrap CIs, 60 replicates):
          Elevation      coverage 100.0% -- borderline
          Amplitude      coverage 75.0% -- INADEQUATE (under-coverage; misses are
                         almost all below the interval: the amplitude CI tends to
                         sit above the truth)
          Displacement   coverage 91.7% -- borderline
        Verdict: CAUTION -- amplitude CIs are less reliable than nominal at this
        sample size. Elevation and displacement coverage rates are borderline at
        this number of replications; a larger `reps` would sharpen the verdict.
        Consider a larger sample or treat near-zero amplitudes as inconclusive
        rather than absent.

