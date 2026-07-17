# print and summary render occasions objects (snapshots)

    Code
      print(res)
    Output
      
      # Profile [T1]:
      
                     Estimate   Lower CI   Upper CI
      Elevation         2.001      1.930      2.067
      X-Value           0.007     -0.006      0.027
      Y-Value           1.487      1.467      1.511
      Amplitude         1.487      1.467      1.511
      Displacement     89.719     88.939     90.245
      Model Fit         1.000                      
      
      
      # Profile [T2]:
      
                     Estimate   Lower CI   Upper CI
      Elevation         2.493      2.422      2.560
      X-Value          -1.057     -1.076     -1.036
      Y-Value           1.078      1.059      1.096
      Amplitude         1.510      1.490      1.528
      Displacement    134.454    133.631    135.079
      Model Fit         1.000                      
      
      
      # Contrast [T2 - T1]:
      
                       Estimate   Lower CI   Upper CI
      Δ Elevation         0.492      0.476      0.508
      Δ X-Value          -1.065     -1.096     -1.040
      Δ Y-Value          -0.409     -0.440     -0.385
      Δ Amplitude         0.023     -0.006      0.045
      Δ Displacement     44.734     43.688     45.670
      Δ Model Fit         0.000                      
      

---

    Code
      summary(res)
    Output
      
      Statistical Basis:	 Mean Scores 
      Bootstrap Resamples:	 100 
      Confidence Level:	 0.95 
      Listwise Deletion:	 TRUE 
      Scale Displacements:	 90 135 180 225 270 315 360 45 
      Occasions:		 T1 T2 
      
      
      # Profile [T1]:
      
                     Estimate   Lower CI   Upper CI
      Elevation         2.001      1.930      2.067
      X-Value           0.007     -0.006      0.027
      Y-Value           1.487      1.467      1.511
      Amplitude         1.487      1.467      1.511
      Displacement     89.719     88.939     90.245
      Model Fit         1.000                      
      
      
      # Profile [T2]:
      
                     Estimate   Lower CI   Upper CI
      Elevation         2.493      2.422      2.560
      X-Value          -1.057     -1.076     -1.036
      Y-Value           1.078      1.059      1.096
      Amplitude         1.510      1.490      1.528
      Displacement    134.454    133.631    135.079
      Model Fit         1.000                      
      
      
      # Contrast [T2 - T1]:
      
                       Estimate   Lower CI   Upper CI
      Δ Elevation         0.492      0.476      0.508
      Δ X-Value          -1.065     -1.096     -1.040
      Δ Y-Value          -0.409     -0.440     -0.385
      Δ Amplitude         0.023     -0.006      0.045
      Δ Displacement     44.734     43.688     45.670
      Δ Model Fit         0.000                      
      

