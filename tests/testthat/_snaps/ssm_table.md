# ssm_table is correct

    Code
      ssm_table(res, render = FALSE)
    Output
        Profile         Elevation           X.Value              Y.Value
      1     All 0.92 (0.89, 0.94) 0.35 (0.32, 0.38) -0.25 (-0.28, -0.22)
                Amplitude         Displacement   Fit
      1 0.43 (0.40, 0.46) 324.3 (320.8, 327.8) 0.878

---

    Code
      ssm_table(res, drop_xy = TRUE, render = FALSE)
    Output
        Profile         Elevation         Amplitude         Displacement   Fit
      1     All 0.92 (0.89, 0.94) 0.43 (0.40, 0.46) 324.3 (320.8, 327.8) 0.878

---

    Code
      ssm_table(res, render = TRUE)
    Output
      <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
      <thead>
      <tr><td colspan='7' style='text-align: left;'>
      Mean-based Structural Summary Statistics with 95% CIs</td></tr>
      <tr>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Profile</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Elevation</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>X.Value</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Y.Value</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Amplitude</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Displacement</th>
      <th style='font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: left;'>Fit</th>
      </tr>
      </thead>
      <tbody>
      <tr>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>All</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.92 (0.89, 0.94)</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.35 (0.32, 0.38)</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>-0.25 (-0.28, -0.22)</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.43 (0.40, 0.46)</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>324.3 (320.8, 327.8)</td>
      <td style='padding-right: 1em; min-width: 3em; white-space: nowrap; border-bottom: 2px solid grey; text-align: left;'>0.878</td>
      </tr>
      </tbody>
      </table>
        Profile         Elevation           X.Value              Y.Value
      1     All 0.92 (0.89, 0.94) 0.35 (0.32, 0.38) -0.25 (-0.28, -0.22)
                Amplitude         Displacement   Fit
      1 0.43 (0.40, 0.46) 324.3 (320.8, 327.8) 0.878

