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
          PA            0     0     0.000     0.000 0.666    0.524    0.809
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
          PA            0     0     0.000     0.000 0.666    0.524    0.809
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
      
        Largest absolute residual: 0.046 (DE – LM)
      
      # Diagnostics
      
        Note: harmonic(s) 3 were on the zero boundary and removed (df adjusted).
      
        Note: analytic (Wald) confidence intervals may materially mis-cover at this sample size
        (N < 2000); prefer the bootstrap on the raw-data path when available.
      

