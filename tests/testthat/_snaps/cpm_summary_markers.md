# analytic summary() output is byte-identical to merge-base (four regimes)

    Code
      summary(clean)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   5000 
      Reference scale:   V1 
      CI method:         analytic 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
       Scale Angle_theory Angle Angle_lci Angle_uci Zeta Zeta_lci Zeta_uci
          V1          360   360   360.000   360.000 0.85    0.778    0.922
          V2           40    40    36.132    43.868 0.70    0.605    0.795
          V3           95    95    90.318    99.682 0.80    0.697    0.903
          V4          150   150   145.366   154.634 0.65    0.567    0.733
          V5          190   190   184.491   195.509 0.75    0.659    0.841
          V6          230   230   222.485   237.515 0.80    0.714    0.886
          V7          285   285   277.370   292.630 0.70    0.597    0.803
          V8          330   330   324.004   335.996 0.60    0.538    0.662
       Communality
             0.722
             0.490
             0.640
             0.423
             0.562
             0.640
             0.490
             0.360
      
      # Correlation-function weights
      
       k Beta Beta_lci Beta_uci
       0 0.35    0.329    0.371
       1 0.30    0.287    0.313
       2 0.20    0.188    0.212
       3 0.15    0.136    0.164
      
      # Fit indices
      
        χ²(10) = 0, p = 1
        RMSEA = 0 [0, 0] (90% CI)
        SRMR  = 0
        CFI   = 1    TLI = 1.009
        AIC   = 36    BIC = 153.309
      
      # Residuals
      
        Largest absolute residual: 0 (V1 – V2)
      

---

    Code
      summary(hey)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     2 
      Sample size (N):   5000 
      Reference scale:   Health 
      CI method:         analytic 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
                    Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci
                   Health            0 360.000        NA        NA 0.955       NA
                  Science           55  50.956        NA        NA 0.828       NA
               Technology          112 105.972        NA        NA 1.000       NA
                   Trades          123 117.996        NA        NA 0.766       NA
       BusinessOperations          192 176.729        NA        NA 0.817       NA
          BusinessContact          210 192.095        NA        NA 0.936       NA
                   Social          269 263.076        NA        NA 1.000       NA
       Zeta_uci Communality
             NA       0.912
             NA       0.685
             NA       1.000
             NA       0.587
             NA       0.668
             NA       0.877
             NA       1.000
      
      # Correlation-function weights
      
       k  Beta Beta_lci Beta_uci
       0 0.608       NA       NA
       1 0.354       NA       NA
       2 0.038       NA       NA
      
      # Fit indices
      
        χ²(6) = 344.015, p = <1e-04
        RMSEA = 0.106 [0.097, 0.116] (90% CI)
        SRMR  = 0.043
        CFI   = 0.984    TLI = 0.945
        AIC   = 374.015    BIC = 471.773
      
      # Residuals
      
        Largest absolute residual: 0.106 (Health – BusinessOperations)
      
      # Diagnostics
      
        Note: a communality index reached its upper boundary (ζ > 0.995, a Heywood-type solution).
      
        Note: this solution is near a parameter boundary or weakly identified
        (Heywood communality; small correlation-function weight; ill-conditioned Hessian);
        analytic (Wald) confidence intervals mis-covered for such fits in validation
        even at N in the tens of thousands. Interpret them with caution and prefer
        the bootstrap on the raw-data path when available.
      

---

    Code
      summary(small)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   300 
      Reference scale:   V1 
      CI method:         analytic 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
       Scale Angle_theory Angle Angle_lci Angle_uci Zeta Zeta_lci Zeta_uci
          V1          360   360   360.000   360.000 0.85    0.555    1.145
          V2           40    40    24.185    55.815 0.70    0.313    1.087
          V3           95    95    75.854   114.146 0.80    0.378    1.222
          V4          150   150   131.052   168.948 0.65    0.312    0.988
          V5          190   190   167.473   212.527 0.75    0.378    1.122
          V6          230   230   199.273   260.727 0.80    0.448    1.152
          V7          285   285   253.802   316.198 0.70    0.279    1.121
          V8          330   330   305.482   354.518 0.60    0.346    0.854
       Communality
             0.722
             0.490
             0.640
             0.423
             0.562
             0.640
             0.490
             0.360
      
      # Correlation-function weights
      
       k Beta Beta_lci Beta_uci
       0 0.35    0.264    0.436
       1 0.30    0.249    0.351
       2 0.20    0.149    0.251
       3 0.15    0.094    0.206
      
      # Fit indices
      
        χ²(10) = 0, p = 1
        RMSEA = 0 [0, 0] (90% CI)
        SRMR  = 0
        CFI   = 1    TLI = 1.167
        AIC   = 36    BIC = 102.668
      
      # Residuals
      
        Largest absolute residual: 0 (V1 – V2)
      
        Note: analytic (Wald) confidence intervals may materially mis-cover at this sample size
        (N < 2000); prefer the bootstrap on the raw-data path when available.
      

---

    Code
      summary(free)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   5000 
      Reference scale:   V1 
      CI method:         analytic 
      Confidence level:  0.95 
      
      # Estimated angles and communality indices
      
       Scale Angle_theory Angle Angle_lci Angle_uci Zeta Zeta_lci Zeta_uci
          V1          360   360   360.000   360.000 0.85    0.778    0.922
          V2           40    40    36.129    43.871 0.70    0.605    0.795
          V3           95    95    90.316    99.684 0.80    0.696    0.904
          V4          150   150   145.363   154.637 0.65    0.567    0.733
          V5          190   190   184.489   195.511 0.75    0.659    0.841
          V6          230   230   222.481   237.519 0.80    0.713    0.887
          V7          285   285   277.366   292.634 0.70    0.596    0.804
          V8          330   330   323.998   336.002 0.60    0.537    0.663
       Communality VarRatio
             0.722        1
             0.490        1
             0.640        1
             0.423        1
             0.563        1
             0.640        1
             0.490        1
             0.360        1
      
      # Correlation-function weights
      
       k Beta Beta_lci Beta_uci
       0 0.35    0.329    0.371
       1 0.30    0.287    0.313
       2 0.20    0.188    0.212
       3 0.15    0.136    0.164
      
      # Fit indices
      
        χ²(10) = 0, p = 1
        RMSEA = 0 [0, 0] (90% CI)
        SRMR  = 0
        CFI   = 1    TLI = 1.009
        AIC   = 52    BIC = 221.447
      
      # Residuals
      
        Largest absolute residual: 0 (V2 – V3)
      
        Note: the free-scaling variance ratios (σ²) carry no confidence interval.
      

# print() on a bootstrap marker-firing fit is byte-identical to merge-base

    Code
      print(jz)
    Output
      
      Circular Process Model (Browne, 1992) 
      Model:             quasi-circumplex 
      Harmonics (m):     3 
      Sample size (N):   1166 
      Reference scale:   PA 
      
       Scale Angle_theory   Angle Angle_lci Angle_uci  Zeta Zeta_lci Zeta_uci
          PA           90  90.000    90.000    90.000 0.767    0.677    0.855
          BC          135 125.074   116.313   136.233 0.931    0.890    1.000
          DE          180 170.353   161.076   186.176 0.780    0.744    0.816
          FG          225 195.425   186.836   206.213 0.861    0.818    0.892
          HI          270 250.721   241.576   260.416 0.956    0.938    0.977
          JK          315 269.491   261.221   281.858 0.942    0.930    0.960
          LM          360 294.230   286.797   304.517 0.806    0.764    0.852
          NO           45  11.305     1.764    20.669 1.000    1.000    1.000
       Communality
             0.589
             0.868
             0.608
             0.741
             0.914
             0.888
             0.650
             1.000
      
      Fit: χ²(10) = 81.169, p = <1e-04; RMSEA = 0.078 [0.063, 0.094]; SRMR = 0.042; CFI = 0.984
        Note: a communality index reached its upper boundary (ζ > 0.995, a Heywood-type solution).

