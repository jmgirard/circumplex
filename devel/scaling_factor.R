scaling_factor <- function(R, N, r) {
  m <- 3 # number of Fourier betas
  factr <- 1e9  # convergence criterion
  pgtol <- 0 # convergence gradient tolerance
  iterlim <- 250 # maximum number of iterations
  
  p <- dim(R)[1] # number of scales
  k <- 3 # same as m?
  K <- pi / 180 # degrees to radians conversion factor
  
  ifa <- function(rr, mm) {
    if (length(which(eigen(rr)$values < 0)) != 0) {
      stop("Make sure the listwise, not pairwise, missing data treatment has been selected in computing the input matrix\n")
    }
    rinv <- solve(rr) 
    sm2i <- diag(rinv)
    smrt <- sqrt(sm2i)
    dsmrt <- diag(smrt)
    rsr <- dsmrt %*% rr %*% dsmrt
    reig <- eigen(rsr)
    vlamd <- reig$va
    vlamdm <- vlamd[1:mm]
    qqm <- as.matrix(reig$ve[, 1:mm])
    theta <- mean(vlamd[(mm + 1):nrow(qqm)])
    dg <- sqrt(vlamdm - theta)
    fac <- diag(1/smrt) %*% qqm %*% diag(dg)
    list(vlamd = vlamd, theta = theta, fac = fac)
  }
  
  start.valuesA <- function(R, k, start.value = start.values){
    one <- matrix(1, p, 1)
    Lambda <- ifa(R, k)$fac
    Diagzeta <- diag(diag(Lambda %*% t(Lambda)))
    Dzeta <- sqrt(Diagzeta)
    uniq <- diag(1, p, p) - (Lambda %*% t(Lambda))
    Dpsi <- diag(diag(uniq))
    Dv <- solve(Dzeta) %*% Dpsi
    Lambdax <- solve(Dzeta) %*% Lambda
    Lambdaxhat <- 1 / p * (t(Lambdax) %*% one)
    C <- 1 / p * t(Lambdax - one %*% t(Lambdaxhat)) %*% (Lambdax - one %*% t(Lambdaxhat))
    U <- eigen(C)$vectors
    Lambdatilde <- Lambdax %*% U
    polar.angles <- rep(0, p)
    for (i in 1:p) {
      polar.angles[i] <- (i - 1) * (2 * pi) / p
    }
    polar.angles <- polar.angles / K
    betas <- matrix(0, 1, m + 1)
    betas[, 1] <- (1 / p * sum(Lambdatilde[, 3]))^2
    betas[, 2] <- 1 - betas[, 1]
    if (m > k) {
      betas[, 3:m] <- 0
    }
    if (m == k) {
      betas[, 3] <- 0
    }
    betas = betas / betas[, 2]
  
    z <- sqrt(diag(Lambda %*% t(Lambda)))
    uniq <- diag(1, p, p) - (diag(z^2))
    Dpsi <- diag(diag(uniq))
    Dv <- solve(Dzeta) %*% Dpsi
    v <- diag(Dv)
    v <- mean(v)
    par <- c(c(betas[-c(2)]), v, z)
    attributes(par) <- list(parA = par, polar.angles = polar.angles, betas = betas)
  }
  
  parA <- start.valuesA(R, k)$parA
  betas <- start.valuesA(R, k)$betas
  
  up <- rep(Inf, length(parA))
  low <- c(rep(0, length(parA)))
  
  objective3max <- function(par) {
    ang <- c(start.valuesA(R, k)$polar.angles) * K
    alpha <- c(par[(1)], 1, par[(2):((m))])
    b <- alpha / sum(alpha)
    v <- par[((m) + 1)]
    z <- par[((m) + 1 + 1):((m) + 1 + p)]
    K <- pi / 180
    M <- matrix(c(0), p, p, byrow = TRUE)
    for(i in 1:p){
      for(j in 1:p){
        M[i, j] <- c(b[-c(1)]) %*% cos(c(1:m) * (ang[j] - ang[i]))
      }
    }
    Pc <- M + matrix(b[1], p, p)
    Dv <- diag(v, p)
    Dz <- diag(z)
    f <- -1 * (sum(diag(R %*% solve(Dz %*% (Pc + Dv) %*% Dz))) + log(det(Dz %*% (Pc + Dv) %*% Dz)) - log(det(R)) - p)
    S1 <- Dz %*% (Pc + Dv) %*% Dz
    S2 <- diag(1 / sqrt(diag(S1))) %*% Dz
    S <- S2 %*% (Pc + Dv) %*% S2
    attributes(f) <- list(f = f, S = S, Cs = Dz %*% (Pc + Dv) %*% Dz, Pc = Pc)
    f
  }
  
  objective3gr <- function(par) {
    ang <- c(start.valuesA(R, k)$polar.angles) * K
    alpha <- c(par[(1)], 1, par[(2):((m))])
    b <- alpha / sum(alpha)
    v <- par[((m) + 1)]
    z <- par[((m) + 1 + 1):((m) + 1 + p)]
    K <- pi / 180
    M <- matrix(c(0), p, p, byrow = TRUE)
    for (i in 1:p) {
      for (j in 1:p) {
        M[i, j] <- c(b[-c(1)]) %*% cos(c(1:m) * (ang[j] - ang[i]))
      }
    }
    Pc <- M + matrix(b[1], p, p)
    Dv <- diag(v, p)
    Dz <- diag(z) 
    S <- Dz %*% (Pc + Dv) %*% Dz
    hessian <- matrix(0, length(par), length(par))
    gradientz <- rep(0, p)
    Dpz <- matrix(0, p * p, p)
    for (i in 1:p) {
      J <- matrix(0, p, p)
      J[i, i] <- 1
      dp <- J %*% (Pc + Dv) %*% Dz + Dz %*% (Pc + Dv) %*% J
      gradientz[i] <- 1 * sum(diag(solve(S) %*% (R - S) %*% solve(S) %*% dp))
      Dpz[, i] <- c(dp)
    }
    gradientv <- rep(0, p)
    Dpv <- matrix(0, p * p, 1)
    J <- diag(1, p, p) 
    dp <- J %*% (Dz)^2
    gradientv <- 1 * sum(diag(solve(S) %*% (R - S) %*% solve(S) %*% dp))
    Dpv <- c(dp)
    gradientalph <- rep(0, (m + 1))
    Dpalph <- matrix(0, p * p, length(alpha))
    for (i in 2:(m + 1)) {
      M <- matrix(c(0), p, p, byrow = TRUE)
      for (z in 1:p) {
        for (j in 1:p) {
          M[z, j] <- ((1 / sum(alpha) - alpha[i] / sum(alpha)^2) * cos((i - 1) * (ang[j] - ang[z])) - 1 * (alpha[1] / sum(alpha)^2 + (alpha[-c(1)] / sum(alpha))^2 %*% cos(c(1:m) * (ang[j] - ang[z])))) * Dz[z, z] * Dz[j, j] 
        }
      }
      dp <- M
      gradientalph[i] <- 1 * sum(diag(solve(S) %*% (R - S) %*% solve(S) %*% dp))
      Dpalph[, i] <- c(dp)
    }
    for (i in 1:1) {
      M <- matrix(c(0), p, p, byrow = TRUE)
      for (z in 1:p) {
        for (j in 1:p) {
          M[z, j] <-  (1 / sum(alpha) - alpha[i] / sum(alpha)^2 - 1 * sum((alpha[-c(1)] / sum(alpha)^2) * cos(c(1:m) * (ang[j] - ang[z])))) * Dz[z, z] * Dz[j, j]
        }}
      dp <- M
      gradientalph[i]<- 1 * sum(diag(solve(S) %*% (R - S) %*% solve(S) %*% dp))
      Dpalph[, i] <- c(dp)
    }
    gradient <- c(gradientalph[-c(2)],gradientv,gradientz)        
    gradient
  }
  
  maxL.BFGS.B <- function(fn, grad = NULL, start, up, low, ...) {
    nParam <- length(start)
    func <- function(theta, ...) {
      sum(fn(theta, ...))
    }
    gradient <- function(theta, ...) {
      if(!is.null(grad)) {
        g <- grad(theta, ...)
        if(!is.null(dim(g))) {
          if(ncol(g) > 1) {
            return(colSums(g))
          }
        } else {
          return(g)
        }
      }
      g <- numericGradient(fn, theta, ...)
      if(!is.null(dim(g))) {
        return(colSums(g))
      } else {
        return(g)
      }
    }
    G1 <- gradient(start, ...)
    if(any(is.na(G1))) {
      stop("NA in the initial gradient")
    }
    if(length(G1) != nParam) {
      stop("length of gradient (", length(G1), 
           ") not equal to the no. of parameters (", nParam, ")")
    }
    control <- list(
      trace = 0,
      REPORT = 1,
      fnscale = -1,
      maxit = iterlim,
      factr = factr,
      pgtol = pgtol,
      lmm = length(parA)
    )
    a <- optim(
      start, 
      func, 
      gr = gradient,
      control = control,
      method = "L-BFGS-B",
      hessian = FALSE,
      upper = up,
      lower = low,
      ...
    )
    result <- list(
      maximum = a$value,
      estimate = a$par
    )
    result
  }
  
  res <- try(maxL.BFGS.B(objective3max, grad = objective3gr, start = parA, up, low))
  if (is.character(res) == TRUE | res$maximum > 0) {
    f_a <- NA_real_
  } else {
    param <- res$estimate
    S <- attr(objective3max(param), "S") 
    rho <- S[2:5, 1]
    f_a <- (1 / 2) * sqrt(sqrt(2) * (rho[1] - rho[3]) + (1 - rho[4]))
  }
  f_a
}