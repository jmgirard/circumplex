# print shows the fit call and the extraction line for each engine

    Code
      print(ssm_growth_formula("glmmTMB"))
    Output
      Joint growth model on SSM coordinates, glmmTMB dialect.
      Fit on the long table from ssm_growth_data(), then keep the fixed effects:
      
      fit <- glmmTMB::glmmTMB(
        value ~ 0 + dv + dv:wave + us(0 + dv | person),
        dispformula = ~ 0 + dv,
        data = long,
        REML = TRUE
      )
      coef <- glmmTMB::fixef(fit)$cond
      vcov <- as.matrix(vcov(fit)$cond)

---

    Code
      print(ssm_growth_formula("nlme"))
    Output
      Joint growth model on SSM coordinates, nlme dialect.
      Fit on the long table from ssm_growth_data(), then keep the fixed effects:
      
      fit <- nlme::lme(
        fixed = value ~ 0 + dv + dv:wave,
        random = ~ 0 + dv | person,
        weights = nlme::varIdent(form = ~ 1 | dv),
        data = long,
        method = "REML"
      )
      coef <- nlme::fixef(fit)
      vcov <- as.matrix(vcov(fit))

---

    Code
      print(ssm_growth_formula("brms"))
    Output
      Joint growth model on SSM coordinates, brms dialect.
      Fit on the long table from ssm_growth_data(), then keep the draws:
      
      fit <- brms::brm(
        brms::bf(
          value ~ 0 + dv + dv:wave + (0 + dv | person),
          sigma ~ 0 + dv
        ),
        data = long
      )
      draws <- as.matrix(fit)

