# print and summary render as expected

    Code
      print(res)
    Output
      
      Circumplex Structure Tests (Acton & Revelle, 2004)
      Scales (nv):  8
      Scoring:      deviation (row-mean centered)
      
      # Exploratory criteria
      
       Test     Statistic Interpretation                                           
       Fisher   0.102     equal axes: at least 3x as likely as the alternative     
       Gap      0.152     equal spacing: at least 3x as likely as the alternative  
       Variance 0.180     interstitiality: almost certain                          
       Rotation 0.325     interstitiality: at least 3x as likely as the alternative
      
      # Order hypothesis (RANDALL)
      
        Correspondence index = 0.868, p = 0.000397 (exact, 5040 relabelings)
      
        Interpretations are heuristic likelihood classifications from simulation, not
        significance tests (Acton & Revelle, 2004). RANDALL's p-value is exact.

---

    Code
      summary(res)
    Output
      
      Circumplex Structure Tests (Acton & Revelle, 2004)
      Scales (nv):  8
      Scoring:      deviation (row-mean centered)
      Ridge:        0
      
      # Exploratory criteria
      
       Test     Statistic Almost Thrice Twice Verdict       
       Fisher   0.102     0.07   0.12   0.15  3x+ likely    
       Gap      0.152     0.15   0.40   0.46  3x+ likely    
       Variance 0.180     0.19   0.59   0.64  almost certain
       Rotation 0.325     0.32   0.64   0.67  3x+ likely    
      
      # Estimated scale geometry
      
       Scale Angle   Communality
       PA    339.635 0.642      
       BC    359.858 0.571      
       DE     48.584 0.500      
       FG     81.337 0.522      
       HI    161.662 0.690      
       JK    183.339 0.713      
       LM    215.503 0.388      
       NO    287.119 0.474      
      
      # Order hypothesis (RANDALL)
      
        Correspondence index = 0.868, p = 0.000397 (exact, 5040 relabelings)
      
        Interpretations are heuristic likelihood classifications from simulation, not
        significance tests (Acton & Revelle, 2004). RANDALL's p-value is exact.
      

