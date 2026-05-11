# =====================================================
# PARAMETERE OG KALIBRERING
# =====================================================
# Dette scriptet definerer alle modellparametere og
# kalibrerer etterspørsel og tilbud basert på data.
# =====================================================


# -----------------------------------------------------
# DEFINERER PARAMETERE
# -----------------------------------------------------

params <- list(
  c = NA, d = NA,
  a_N = NA, b_N = NA, alpha_N = NA, tau_N = NA,
  a_U = NA, b_U = NA, alpha_U = NA, tau_U = NA,
  f_N = NA, f_U = NA, a_EL = NA, b_EL = NA, s_EL = NA
)


# -----------------------------------------------------
# DATA (Basisår 2024)
# -----------------------------------------------------

X_N_data <- 23.6
X_U_data <- 8.1
P_data <- 2.89

X_total <- X_N_data + X_U_data

# -----------------------------------------------------
# KALIBRERING AV ETTERSPØRSEL
# -----------------------------------------------------
# Lineær etterspørsel: P = c - dQ
# Elastisitet brukes til å identifisere helning

# Elastisitet
e <- -0.6

# Beregner d
params$d <- -P_data / (X_total * e)

# Beregner c
params$c <- P_data + params$d * X_total


# -----------------------------------------------------
# KOSTNADER
# -----------------------------------------------------

# Dieselpris i hhv. Norge og utlandet
params$f_N <- 0.40349
params$f_U <- 0.39642

# CO2-pris
params$tau_N <- 0.05453
params$tau_U <- 0.03329

# Kostnader uten drivstoffkostnader
params$a_N <- 1.78716
params$a_U <- 1.23922
params$a_EL <- 2.41465

# Andel fylt i Norge
params$alpha_N <- 0.90
params$alpha_U <- 0.12


# -----------------------------------------------------
# KALIBRERING AV b
# -----------------------------------------------------

# Konstantledd
const_N <- params$a_N +
  params$alpha_N * (params$f_N + params$tau_N) +
  (1 - params$alpha_N) * (params$f_U + params$tau_U)

const_U <- params$a_U +
  params$alpha_U * (params$f_N + params$tau_N) +
  (1 - params$alpha_U) * (params$f_U + params$tau_U)

# Kalibrerer b_N og b_U direkte fra MC = P
params$b_N <- (P_data - const_N) / X_N_data
params$b_U <- (P_data - const_U) / X_U_data

# Antar at b_EL = b_N 
params$b_EL <- params$b_N

# -----------------------------------------------------
# SUBSIDIE
# -----------------------------------------------------

params$s_EL <- 0.35326

# -----------------------------------------------------
# ETS2-PRIS
# -----------------------------------------------------

params$ETS2 <- 0.05170

# ETS2-pris i 2040 (scenario 3):
# Antatt startpris i 2028, vokser med 5% per år
ETS2_vekst       <- 0.05
ETS2_år          <- 2040 - 2028
params$ETS2_2040 <- params$ETS2 * (1 + ETS2_vekst)^ETS2_år

