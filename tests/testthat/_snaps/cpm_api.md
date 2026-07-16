# print and summary render as expected

    Code
      print(fit)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     2 
      Sample size (N):   300 
      Reference scale:   PA 
      
       Scale Angle_theory Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
          PA            0   360   360.000   360.000 0.666    0.524    0.809
          BC           45    45    20.816    69.184 0.666    0.524    0.809
          DE           90    90    60.671   119.329 0.666    0.524    0.809
          FG          135   135   105.425   164.575 0.666    0.524    0.809
          HI          180   180   150.307   209.693 0.666    0.524    0.809
          JK          225   225   195.425   254.575 0.666    0.524    0.809
          LM          270   270   240.671   299.329 0.666    0.524    0.809
          NO          315   315   290.816   339.184 0.666    0.524    0.809
       Communality
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
      
      Fit: χ²(11) = 23.059, p = 0.0173; RMSEA = 0.061 [0.024, 0.095]; SRMR = 0.03; CFI = 0.965
        Note: harmonic(s) 3 were on the zero boundary and removed (df adjusted).

---

    Code
      summary(fit)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     2 
      Sample size (N):   300 
      Reference scale:   PA 
      CI method:         analytic 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
       Scale Angle_theory Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
          PA            0   360   360.000   360.000 0.666    0.524    0.809
          BC           45    45    20.816    69.184 0.666    0.524    0.809
          DE           90    90    60.671   119.329 0.666    0.524    0.809
          FG          135   135   105.425   164.575 0.666    0.524    0.809
          HI          180   180   150.307   209.693 0.666    0.524    0.809
          JK          225   225   195.425   254.575 0.666    0.524    0.809
          LM          270   270   240.671   299.329 0.666    0.524    0.809
          NO          315   315   290.816   339.184 0.666    0.524    0.809
       Communality
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
             0.444
      
      # Correlation-function weights
      
       k  Beta Beta_lci Beta_uci
       0 0.556    0.483    0.630
       1 0.359    0.294    0.424
       2 0.085    0.036    0.133
       3 0.000    0.000    0.000
      
      # Fit indices
      
        χ²(11) = 23.059, p = 0.0173
        RMSEA = 0.061 [0.024, 0.095] (90% CI)
        SRMR  = 0.03
        CFI   = 0.965    TLI = 0.911
        AIC   = 57.059    BIC = 120.023
      
      # Residuals
      
        Largest absolute residual: 0.046 (PA – HI)
      
      # Diagnostics
      
        Note: harmonic(s) 3 were on the zero boundary and removed (df adjusted).
      
        Note: analytic (Wald) confidence intervals may materially mis-cover at this sample size
        (N < 2000); prefer the bootstrap on the raw-data path when available.
      

# print and summary render a bootstrap fit as expected

    Code
      print(fit)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   300 
      Reference scale:   PA 
      
       Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
          PA            0 360.000   360.000   360.000 0.585    0.444    0.763
          BC           45  29.176     7.249    52.683 0.806    0.605    1.000
          DE           90  97.256    69.267   114.967 0.986    0.663    1.000
          FG          135 159.337   122.910   185.096 0.840    0.614    1.000
          HI          180 206.234   157.127   241.945 0.766    0.613    1.000
          JK          225 246.497   203.138   285.521 0.580    0.466    0.956
          LM          270 283.823   241.376   311.817 0.836    0.672    1.000
          NO          315 329.632   308.990   346.872 0.607    0.442    1.000
       Communality
             0.342
             0.650
             0.973
             0.705
             0.587
             0.337
             0.700
             0.369
      
      Fit: χ²(10) = 2.462, p = 0.991; RMSEA = 0 [0, 0]; SRMR = 0.015; CFI = 1
        Note: 1 of 100 bootstrap resamples were excluded (0 degenerate, 1 non-convergent); the intervals are based on 99 replicates and are conditional on estimability.

---

    Code
      summary(fit)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   300 
      Reference scale:   PA 
      CI method:         bootstrap 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
       Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
          PA            0 360.000   360.000   360.000 0.585    0.444    0.763
          BC           45  29.176     7.249    52.683 0.806    0.605    1.000
          DE           90  97.256    69.267   114.967 0.986    0.663    1.000
          FG          135 159.337   122.910   185.096 0.840    0.614    1.000
          HI          180 206.234   157.127   241.945 0.766    0.613    1.000
          JK          225 246.497   203.138   285.521 0.580    0.466    0.956
          LM          270 283.823   241.376   311.817 0.836    0.672    1.000
          NO          315 329.632   308.990   346.872 0.607    0.442    1.000
       Communality
             0.342
             0.650
             0.973
             0.705
             0.587
             0.337
             0.700
             0.369
      
      # Correlation-function weights
      
       k  Beta Beta_lci Beta_uci
       0 0.438    0.369    0.519
       1 0.342    0.286    0.377
       2 0.172    0.136    0.219
       3 0.047    0.000    0.110
      
      # Fit indices
      
        χ²(10) = 2.462, p = 0.991
        RMSEA = 0 [0, 0] (90% CI)
        SRMR  = 0.015
        CFI   = 1    TLI = 1.069
        AIC   = 38.462    BIC = 105.13
      
      # Residuals
      
        Largest absolute residual: 0.028 (BC – HI)
      
      # Diagnostics
      
        Note: 1 of 100 bootstrap resamples were excluded (0 degenerate, 1 non-convergent); the intervals are based on 99 replicates and are conditional on estimability.
      

