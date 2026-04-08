# =====================================================
# LØSNING AV LIKEVEKT
# =====================================================
# Finner likevektspris og mengder ved å løse modellen
# =====================================================


# -----------------------------------------------------
# Solver for scenarioer: referanse, 1 og 2
# -----------------------------------------------------

solve_equilibrium <- function(params) {
  
  # Objective-funksjon (minimerer avvik)
  objective <- function(X) {
    
    X_N <- X[1]
    X_U <- X[2]
    
    P <- Demand(X_N, X_U, 0, params)
    
    eq1 <- P - MC_N(X_N, params)
    eq2 <- P - MC_U(X_U, params)
    
    return(eq1^2 + eq2^2)
  }
  
  # Startverdier (nær observerte verdier)
  start <- c(23, 8)
  
  result <- optim(start, objective)
  
  X_N <- result$par[1]
  X_U <- result$par[2]
  P <- Demand(X_N, X_U, 0, params)
  
  return(c(P = P, X_N = X_N, X_U = X_U, X_EL = 0))
}

# -----------------------------------------------------
# Solver for scenario 3
# -----------------------------------------------------

solve_equilibrium_EL <- function(params) {
  
  objective <- function(X) {
    
    X_N <- X[1]
    X_U <- X[2]
    X_EL <- X[3]
    
    P <- Demand(X_N, X_U, X_EL, params)
    
    eq1 <- P - MC_N(X_N, params)
    eq2 <- P - MC_U(X_U, params)
    eq3 <- P - MC_EL(X_EL, params)
    
    return(eq1^2 + eq2^2 + eq3^2)
  }
  
  start <- c(23, 8, 1)
  
  result <- optim(start, objective)
  
  X_N <- result$par[1]
  X_U <- result$par[2]
  X_EL <- result$par[3]
  P <- Demand(X_N, X_U, X_EL, params)
  
  return(c(P = P, X_N = X_N, X_U = X_U, X_EL = X_EL))
}