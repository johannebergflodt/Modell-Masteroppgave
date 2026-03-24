# -------------------------
# PARAMETERE
# -------------------------

params <- list(
  
  # Etterspørsel
  c = NA,   # skjæringspunkt
  d = NA,   # helning
  
  # Kostnader Norge
  a_N = NA,
  b_N = NA,
  alpha_N = NA,
  tau_N = NA,
  
  # Kostnader utland
  a_U = NA,
  b_U = NA,
  alpha_U = NA,
  tau_U = NA,
  
  # Drivstoff
  f_N = NA,
  f_U = NA
)

# -------------------------
# TILBUD (Marginalkostnad)
# -------------------------

# Norge
MC_N <- function(X_N, params) {
  params$a_N + 
    params$alpha_N * params$f_N + 
    (1 - params$alpha_N) * params$f_U + 
    params$tau_N + 
    params$b_N * X_N
}

# Utlandet
MC_U <- function(X_U, params) {
  params$a_U + 
    params$alpha_U * params$f_N + 
    (1 - params$alpha_U) * params$f_U + 
    params$tau_U + 
    params$b_U * X_U
}

# -------------------------
# ETTERSPØRSEL
# -------------------------

Demand <- function(X_N, X_U, params) {
  params$c - params$d * (X_N + X_U)
}

# -------------------------
# DATA (observasjoner)
# -------------------------

X_N_data <- 24.4
X_U_data <- 7.9
P_data <- 2.19

X_total <- X_N_data + X_U_data

# -------------------------
# KALIBRERING (Etterspørsel)
# -------------------------

# Elastisitet
e <- -0.5

# Beregn d
d <- - P_data / (X_total * e)

# lagre i params
params$d <- d

# Beregn c
c_val <- P_data + params$d * X_total
params$c <- c_val

# -------------------------
# KOSTNADER (ikke ferdig)
# -------------------------

# Diesel (kr per tonnkm)
params$f_N <- 0.458
params$f_U <- 0.397

# -------------------------
# SCENARIOER
# -------------------------

# BaU (baseline)
scenario_BaU <- function(params) {
  params
}

# SCENARIO 1: ETS2 innføres
scenario_ETS2 <- function(params) {
  params_copy <- params
  
  ETS2_price <- 1115   # kr per tonn CO2
  
  # Foreløpig: legger direkte til (EST2 justeres senere til kr per tonnkm?)
  params_copy$tau_N <- params_copy$tau_N + ETS2_price
  params_copy$tau_U <- params_copy$tau_U + ETS2_price
  
  return(params_copy)
}

# SCENARIO 2: ETS2 + CO2-avgift
scenario_ETS2_CO2 <- function(params) {
  params_copy <- params
  
  ETS2_price <- 1115
  
  # Norge: begge
  params_copy$tau_N <- params_copy$tau_N + ETS2_price
  
  # Utlandet: kun ETS2 (forenkling)
  params_copy$tau_U <- ETS2_price
  
  return(params_copy)
}