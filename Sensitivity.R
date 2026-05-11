# =======================================================
# SENSITIVITETSANALYSE
# =======================================================
# Dette scriptet tester hvordan resultatene endres ved 
# variasjon i sentrale parametere


# ========================================================
# PRISELASTISITET
# ========================================================

# --------------------------------------------------------
# ELASTISITETER SOM TESTES
# --------------------------------------------------------
# Basis: -0,6 (benyttet i modellen)
# Lav:   -0,2 (svært uelastisk, jf. Oslo Economics)
# Høy:   -1,5 (elastisk, jf. De Jong et al. 2010)

elastisiteter <- c(-0.2, -0.6, -1.5)

# --------------------------------------------------------
# FUNKSJON: Rekalibrerer d og c for ny elastisitet
# --------------------------------------------------------
rekaliber_etterspørsel <- function(params, e_ny) {
  params_ny <- params
  params_ny$d <- -P_data / (X_total * e_ny)
  params_ny$c <- P_data + params_ny$d * X_total
  return(params_ny)
}

# --------------------------------------------------------
# KJØRER MODELLEN FOR HVER ELASTISITET
# --------------------------------------------------------
resultater_sens <- data.frame()

for (e in elastisiteter) {
  
  # Rekalibrerte parametere
  params_e <- rekaliber_etterspørsel(params, e)
  
  # Løser referansescenario
  rs_e <- solve_equilibrium(params_e)
  
  # Løser scenario 1
  s1_e <- solve_equilibrium(scenario_1_ETS2(params_e))
  
  # Prosentvis endring fra referansescenario
  pst_P_s1      <- (s1_e["P"]                    - rs_e["P"])                    / rs_e["P"]                    * 100
  pst_XN_s1     <- (s1_e["X_N"]                  - rs_e["X_N"])                  / rs_e["X_N"]                  * 100
  pst_XU_s1     <- (s1_e["X_U"]                  - rs_e["X_U"])                  / rs_e["X_U"]                  * 100
  pst_XTotal_s1 <- (s1_e["X_N"] + s1_e["X_U"]   - rs_e["X_N"] - rs_e["X_U"])   / (rs_e["X_N"] + rs_e["X_U"]) * 100
  
  # Samler resultater
  resultater_sens <- rbind(resultater_sens, data.frame(
    elastisitet   = e,
    # Likevektsverdier scenario 1
    s1_P          = round(s1_e["P"],                    4),
    s1_XN         = round(s1_e["X_N"],                  4),
    s1_XU         = round(s1_e["X_U"],                  4),
    s1_XTotal     = round(s1_e["X_N"] + s1_e["X_U"],    4),
    # Prosentvis endring fra referanse
    pst_P_s1      = round(pst_P_s1,      4),
    pst_XN_s1     = round(pst_XN_s1,     4),
    pst_XU_s1     = round(pst_XU_s1,     4),
    pst_XTotal_s1 = round(pst_XTotal_s1, 4)
  ))
}

# --------------------------------------------------------
# UTSKRIFT
# --------------------------------------------------------
cat("\n=== SENSITIVITETSANALYSE: PRISELASTISITET ===\n\n")

cat("--- Scenario 1: ETS2 innføres ---\n")
tabell_s1 <- data.frame(
  Elastisitet = c("-0.2", "-0.6 (basis)", "-1.5"),
  P           = resultater_sens$s1_P,
  X_N         = resultater_sens$s1_XN,
  X_U         = resultater_sens$s1_XU,
  X_Total     = resultater_sens$s1_XTotal,
  dP          = resultater_sens$pst_P_s1,
  dX_N        = resultater_sens$pst_XN_s1,
  dX_U        = resultater_sens$pst_XU_s1,
  dX_Total    = resultater_sens$pst_XTotal_s1
)
names(tabell_s1) <- c("Elastisitet", "P", "X_N", "X_U", "X_Total",
                      "ΔP (%)", "ΔX_N (%)", "ΔX_U (%)", "ΔX_Total (%)")
print(tabell_s1, row.names = FALSE)


# ========================================================
# IKKE-DRIVSTOFFRELATERTE KOSTNADER
# ========================================================

# -------------------------------------------------------
# a-ENE SOM TESTES
# -------------------------------------------------------
# Basis:  a_N = 1.78716, a_U = 1.23922
# Lavere: reduksjon på 58% (lengre kjørelengde: 82 500 km)
# Høyere: økning på 30% (høyere kostnadsnivå)

a_N_lav  <- 0.75793
a_U_lav  <- 0.52555
a_N_høy  <- 2.32331
a_U_høy  <- 1.61098

# -------------------------------------------------------
# FUNKSJON: Rekalibrerer b for nye a-verdier
# -------------------------------------------------------
rekaliber_b <- function(params, a_N_ny, a_U_ny) {
  params_ny <- params
  
  params_ny$a_N <- a_N_ny
  params_ny$a_U <- a_U_ny
  
  const_N <- a_N_ny +
    params$alpha_N * (params$f_N + params$tau_N) +
    (1 - params$alpha_N) * (params$f_U + params$tau_U)
  const_U <- a_U_ny +
    params$alpha_U * (params$f_N + params$tau_N) +
    (1 - params$alpha_U) * (params$f_U + params$tau_U)
  
  params_ny$b_N  <- (P_data - const_N) / X_N_data
  params_ny$b_U  <- (P_data - const_U) / X_U_data
  params_ny$b_EL <- params_ny$b_N
  
  return(params_ny)
}

# -------------------------------------------------------
# KJØRER MODELLEN FOR LAVERE OG HØYERE a
# -------------------------------------------------------
params_lav <- rekaliber_b(params, a_N_lav, a_U_lav)
rs_lav  <- solve_equilibrium(scenario_rs(params_lav))
s1_lav  <- solve_equilibrium(scenario_1_ETS2(params_lav))

params_høy <- rekaliber_b(params, a_N_høy, a_U_høy)
rs_høy  <- solve_equilibrium(scenario_rs(params_høy))
s1_høy  <- solve_equilibrium(scenario_1_ETS2(params_høy))

# -------------------------------------------------------
# PROSENTVIS ENDRING FRA REFERANSESCENARIO
# -------------------------------------------------------
pst_a_P_s1_lav      <- (s1_lav["P"]   - rs_lav["P"])   / rs_lav["P"]   * 100
pst_a_P_s1_høy      <- (s1_høy["P"]   - rs_høy["P"])   / rs_høy["P"]   * 100
pst_a_XN_s1_lav     <- (s1_lav["X_N"] - rs_lav["X_N"]) / rs_lav["X_N"] * 100
pst_a_XN_s1_høy     <- (s1_høy["X_N"] - rs_høy["X_N"]) / rs_høy["X_N"] * 100
pst_a_XU_s1_lav     <- (s1_lav["X_U"] - rs_lav["X_U"]) / rs_lav["X_U"] * 100
pst_a_XU_s1_høy     <- (s1_høy["X_U"] - rs_høy["X_U"]) / rs_høy["X_U"] * 100
pst_a_XTotal_s1_lav <- (s1_lav["X_N"] + s1_lav["X_U"] - rs_lav["X_N"] - rs_lav["X_U"]) /
  (rs_lav["X_N"] + rs_lav["X_U"]) * 100
pst_a_XTotal_s1_høy <- (s1_høy["X_N"] + s1_høy["X_U"] - rs_høy["X_N"] - rs_høy["X_U"]) /
  (rs_høy["X_N"] + rs_høy["X_U"]) * 100

# Basis (fra hovedanalysen)
pst_a_P_s1_basis      <- (result_s1["P"]   - result_rs["P"])   / result_rs["P"]   * 100
pst_a_XN_s1_basis     <- (result_s1["X_N"] - result_rs["X_N"]) / result_rs["X_N"] * 100
pst_a_XU_s1_basis     <- (result_s1["X_U"] - result_rs["X_U"]) / result_rs["X_U"] * 100
pst_a_XTotal_s1_basis <- (result_s1["X_N"] + result_s1["X_U"] - result_rs["X_N"] - result_rs["X_U"]) /
  (result_rs["X_N"] + result_rs["X_U"]) * 100


# -------------------------------------------------------
# UTSKRIFT
# -------------------------------------------------------
cat("\n=== SENSITIVITETSANALYSE: IKKE-DRIVSTOFFRELATERTE KOSTNADER ===\n\n")

cat("--- Scenario 1: ETS2 innføres ---\n")
tabell_a_s1 <- data.frame(
  a_nivå  = c("Lav (-58%)", "Basis", "Høy (+30%)"),
  P       = round(c(s1_lav["P"],       result_s1["P"],   s1_høy["P"]),   4),
  X_N     = round(c(s1_lav["X_N"],     result_s1["X_N"], s1_høy["X_N"]), 4),
  X_U     = round(c(s1_lav["X_U"],     result_s1["X_U"], s1_høy["X_U"]), 4),
  X_Total = round(c(s1_lav["X_N"] + s1_lav["X_U"],
                    result_s1["X_N"] + result_s1["X_U"],
                    s1_høy["X_N"] + s1_høy["X_U"]), 4),
  dP      = round(c(pst_a_P_s1_lav,      pst_a_P_s1_basis,      pst_a_P_s1_høy),      4),
  dX_N    = round(c(pst_a_XN_s1_lav,     pst_a_XN_s1_basis,     pst_a_XN_s1_høy),     4),
  dX_U    = round(c(pst_a_XU_s1_lav,     pst_a_XU_s1_basis,     pst_a_XU_s1_høy),     4),
  dX_Total = round(c(pst_a_XTotal_s1_lav, pst_a_XTotal_s1_basis, pst_a_XTotal_s1_høy), 4)
)
names(tabell_a_s1) <- c("a-nivå", "P", "X_N", "X_U", "X_Total",
                        "ΔP (%)", "ΔX_N (%)", "ΔX_U (%)", "ΔX_Total (%)")
print(tabell_a_s1, row.names = FALSE)


# -------------------------------------------------------
# REKALIBRERTE b-VERDIER
# -------------------------------------------------------
cat("\n--- Rekalibrerte b-verdier ---\n")
cat("Lav a:  b_N =", round(params_lav$b_N, 5),
    "| b_U =", round(params_lav$b_U, 5), "\n")
cat("Basis:  b_N =", round(params$b_N, 5),
    "| b_U =", round(params$b_U, 5), "\n")
cat("Høy a:  b_N =", round(params_høy$b_N, 5),
    "| b_U =", round(params_høy$b_U, 5), "\n")