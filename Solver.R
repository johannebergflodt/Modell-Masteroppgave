# =====================================================
# LØSNING AV LIKEVEKT
# =====================================================
# Dette scriptet finner likevektsverdiene ved å løse modellen

library(nleqslv)

# -----------------------------------------------------
# Solver for scenarioer: referanse, 1 og 2
# -----------------------------------------------------
solve_equilibrium <- function(params) {
  
  equations <- function(X) {
    X_N <- X[1]
    X_U <- X[2]
    
    P <- Demand(X_N, X_U, 0, params)
    
    eq1 <- P - MC_N(X_N, params)
    eq2 <- P - MC_U(X_U, params)
    
    return(c(eq1, eq2))
  }
  
  result <- nleqslv(c(23, 8), equations)
  
  X_N <- result$x[1]
  X_U <- result$x[2]
  P   <- Demand(X_N, X_U, 0, params)
  
  return(c(P = P, X_N = X_N, X_U = X_U, X_EL = 0))
}

# -----------------------------------------------------
# Solver for scenario 3
# -----------------------------------------------------
solve_equilibrium_EL <- function(params) {
  
  equations <- function(X) {
    X_N  <- X[1]
    X_U  <- X[2]
    X_EL <- X[3]
    
    P <- Demand(X_N, X_U, X_EL, params)
    
    eq1 <- P - MC_N(X_N, params)
    eq2 <- P - MC_U(X_U, params)
    eq3 <- P - MC_EL(X_EL, params)
    
    return(c(eq1, eq2, eq3))
  }
  
  result <- nleqslv(c(23, 8, 1), equations)
  
  X_N  <- result$x[1]
  X_U  <- result$x[2]
  X_EL <- result$x[3]
  P    <- Demand(X_N, X_U, X_EL, params)
  
  return(c(P = P, X_N = X_N, X_U = X_U, X_EL = X_EL))
}