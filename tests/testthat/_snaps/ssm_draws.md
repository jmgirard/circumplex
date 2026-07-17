# print and summary output for draws objects is stable

    Code
      print(res)
    Output
      
      # Posterior Summary:
      
                     Estimate   Lower CrI   Upper CrI
      Elevation         1.050       0.815       1.192
      X-Value           2.750       2.038       3.462
      Y-Value           4.050       3.630       4.378
      Amplitude         4.917       4.804       5.019
      Displacement     55.781      46.356      65.036
      Model Fit                                      
      

---

    Code
      summary(res)
    Output
      
      Statistical Basis:	Posterior Draws 
      Posterior Draws:	 4 
      Credible Level:		 0.95 
      Draw Shape:		 Parameters 
      
      # Posterior Summary:
      
                     Estimate   Lower CrI   Upper CrI
      Elevation         1.050       0.815       1.192
      X-Value           2.750       2.038       3.462
      Y-Value           4.050       3.630       4.378
      Amplitude         4.917       4.804       5.019
      Displacement     55.781      46.356      65.036
      Model Fit                                      
      

---

    Code
      print(resb)
    Output
      
      # Posterior Summary:
      
                     Estimate   Lower CrI   Upper CrI
      Elevation         1.000       1.000       1.900
      X-Value           1.000      -0.800       1.900
      Y-Value           1.000       0.100       1.900
      Amplitude         1.414       1.041       2.687
      Displacement     73.675      45.000     166.500
      Model Fit         1.000                        
      

---

    Code
      summary(resb)
    Output
      
      Statistical Basis:	Posterior Draws 
      Posterior Draws:	 3 
      Credible Level:		 0.9 
      Draw Shape:		 Profiles 
      Scale Displacements:	 90 135 180 225 270 315 360 45 
      
      # Posterior Summary:
      
                     Estimate   Lower CrI   Upper CrI
      Elevation         1.000       1.000       1.900
      X-Value           1.000      -0.800       1.900
      Y-Value           1.000       0.100       1.900
      Amplitude         1.414       1.041       2.687
      Displacement     73.675      45.000     166.500
      Model Fit         1.000                        
      

