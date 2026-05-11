# =====================================================
# MODELLFUNKSJONER
# =====================================================


# -----------------------------------------------------
# TILBUD 
# -----------------------------------------------------

# Norske aktører
MC_N <- function(X_N, params) {
  params$a_N + 
    params$alpha_N * (params$f_N + params$tau_N) + 
    (1 - params$alpha_N) * (params$f_U + params$tau_U) + 
    params$b_N * X_N
}

# Utenlandske aktører
MC_U <- function(X_U, params) {
  params$a_U + 
    params$alpha_U * (params$f_N + params$tau_N) + 
    (1 - params$alpha_U) * (params$f_U + params$tau_U) + 
    params$b_U * X_U
}

# Elektriske aktører
MC_EL <- function(X_EL, params) {
  params$a_EL + params$b_EL * X_EL - params$s_EL
}

# -----------------------------------------------------
# ETTERSPØRSEL
# -----------------------------------------------------

# Funksjonen gjør at X_EL automatisk settes til 0 når
# den ikke inkluderes i de første scenarioene

Demand <- function(X_N, X_U, X_EL = 0, params) {
  params$c - params$d * (X_N + X_U + X_EL)
}