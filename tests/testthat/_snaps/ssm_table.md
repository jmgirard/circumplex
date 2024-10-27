# ssm_table is correct

    Code
      ssm_table(res, render = FALSE)
    Output
        Profile         Elevation           X.Value              Y.Value
      1     All 0.92 (0.89, 0.95) 0.35 (0.32, 0.38) -0.25 (-0.28, -0.22)
                Amplitude         Displacement   Fit
      1 0.43 (0.40, 0.46) 324.3 (320.9, 328.0) 0.878

---

    Code
      ssm_table(res, drop_xy = TRUE, render = FALSE)
    Output
        Profile         Elevation         Amplitude         Displacement   Fit
      1     All 0.92 (0.89, 0.95) 0.43 (0.40, 0.46) 324.3 (320.9, 328.0) 0.878

