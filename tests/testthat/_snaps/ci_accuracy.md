# print and summary snapshots (seeded)

    Code
      print(res)
    Output
      
      SSM CI accuracy, simulated at your n and settings (30 replications per condition; bootstrap intervals with 60 replicates at level 0.95)
      
        # Profile [All] (n = 120; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 100.0% -- borderline
          Amplitude      coverage 96.7% -- borderline
          Displacement   coverage 93.3% when certified -- borderline
          Guardrail      if the true amplitude were zero, displacement would still
                         be certified 100.0% of the time -- the "amplitude CI
                         excludes zero" rule is far weaker than the 2.5% error rate
                         its wording suggests
        Verdict: CAUTION -- the interpretability guardrail provides almost no
        protection against a truly zero amplitude. Elevation, amplitude, and
        certified displacement coverage rates are borderline at this number of
        replications; a larger `reps` would sharpen the verdict. Consider a larger
        sample or treat near-zero amplitudes as inconclusive rather than absent.

---

    Code
      summary(res)
    Output
      
      Statistical Basis:	 Mean Scores 
      Assessed Engine:	 bootstrap with 60 replicates 
      Confidence Level:	 0.95 
      Simulation Reps:	 30 per condition 
      Amplitude Ladder:	 1 0.25 0 
      Population Structure:	 Browne circular model (CPM) 
      Group Sizes:		 All = 120 
      Certification Rule:	 round(a_lci, 3) > 0 (threshold 0.0005 amplitude units) 
      Elapsed:		<masked>
      
      Structure note: population simulated from a Browne circular model fit (m = 3,
      RMSEA = 0.038, SRMR = 0.046).
        The structure fits adequately (RMSEA <= 0.08, Browne & Cudeck, 1993; SRMR
        <= 0.08, Hu & Bentler, 1999), so the simulated population is a reasonable
        stand-in for yours.
        Boundary markers: Heywood communality; small correlation-function weight;
        ill-conditioned Hessian.
      
      CI trustworthiness at the as-estimated condition (c = 1), classified
      against Bradley's (1978) liberal band via 95% Wilson intervals:
      
        # Profile [All] (n = 120; 95% bootstrap CIs, 60 replicates):
          Elevation      coverage 100.0% -- borderline
          Amplitude      coverage 96.7% -- borderline
          Displacement   coverage 93.3% when certified -- borderline
          Guardrail      if the true amplitude were zero, displacement would still
                         be certified 100.0% of the time -- the "amplitude CI
                         excludes zero" rule is far weaker than the 2.5% error rate
                         its wording suggests
        Verdict: CAUTION -- the interpretability guardrail provides almost no
        protection against a truly zero amplitude. Elevation, amplitude, and
        certified displacement coverage rates are borderline at this number of
        replications; a larger `reps` would sharpen the verdict. Consider a larger
        sample or treat near-zero amplitudes as inconclusive rather than absent.
      
      Coverage by profile, parameter, and amplitude condition:
       Profile Parameter Condition Coverage MC_se Left_miss Right_miss Median_width
           All         e      1.00    1.000 0.000     0.000      0.000        0.181
           All         x      1.00    0.967 0.033     0.000      0.033        0.143
           All         y      1.00    0.900 0.055     0.100      0.000        0.131
           All         a      1.00    0.967 0.033     0.000      0.033        0.143
           All         d      1.00    0.933 0.046     0.067      0.000       20.425
           All         e      0.25    0.900 0.055     0.067      0.033        0.179
           All         x      0.25    0.967 0.033     0.033      0.000        0.147
           All         y      0.25    0.867 0.062     0.033      0.100        0.135
           All         a      0.25    0.933 0.046     0.067      0.000        0.130
           All         d      0.25    0.933 0.046     0.033      0.033      106.977
           All         e      0.00    0.900 0.055     0.067      0.033        0.192
           All         x      0.00    0.967 0.033     0.000      0.033        0.136
           All         y      0.00    1.000 0.000     0.000      0.000        0.132
           All         a      0.00    0.000 0.000     1.000      0.000        0.106
           All         d      0.00       NA    NA        NA         NA           NA
       Coverage_conditional N_conditional Structural N_reps
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                      0.933            30      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                      0.933            30      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA      FALSE     30
                         NA            NA       TRUE     30
                         NA             0      FALSE      0
        Note: amplitude coverage on rows flagged Structural is structurally 0 (a
        percentile interval of strictly positive amplitude replicates cannot
        contain a zero truth) -- a theorem, not a measurement; the informative
        near-zero rungs are the small c > 0 ones.
      
      Guardrail operating characteristics:
       Profile Condition Cert_rate Cert_lci Cert_uci Benchmark Caution Threshold
           All      1.00         1    0.886        1     0.025      NA     5e-04
           All      0.25         1    0.886        1     0.025      NA     5e-04
           All      0.00         1    0.886        1     0.025    TRUE     5e-04
       Fit_pass_rate Branch_pathology_rate N_reps
                   1                     0     30
                   0                     0     30
                   0                     0     30

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
                         be certified 100.0% of the time -- the "amplitude CI
                         excludes zero" rule is far weaker than the 2.5% error rate
                         its wording suggests
        Verdict: CAUTION -- amplitude CIs are less reliable than nominal at this
        sample size and the interpretability guardrail provides almost no
        protection against a truly zero amplitude. Elevation and certified
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
                         be certified 100.0% of the time -- the "amplitude CI
                         excludes zero" rule is far weaker than the 2.5% error rate
                         its wording suggests
        Verdict: CAUTION -- amplitude CIs are less reliable than nominal at this
        sample size, displacement CIs mis-cover even when certified, and the
        interpretability guardrail provides almost no protection against a truly
        zero amplitude. Elevation coverage is borderline at this number of
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

